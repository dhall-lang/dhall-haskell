# Shared Dhall Core: Design Document

Status: describes the design as implemented on `feature/shared-dhall-core`
(Phases 0–8 of the shared-core plan). Companion ADRs:

| Doc | Topic |
|-----|--------|
| [001-shared-core.md](001-shared-core.md) | Representation decision (`CoreGraph`) |
| [002-test-matrix.md](002-test-matrix.md) | Invariants and oracles |
| [003-dag-cbor.md](003-dag-cbor.md) | Experimental DAG CBOR |
| [004-format-migration.md](004-format-migration.md) | Future hash/cache switch (not flipped) |

This document is the narrative design: layers, ownership, memo keys, APIs, and
what is intentionally *not* the default.

---

## 1. Problem

The public `Expr` type is a source-facing **tree**: each constructor owns its
children. Evaluation and quoting therefore copy structure. A small source
program can produce a huge normal form in which the same union type or record
shape is allocated thousands of times. Peak memory and many walks then scale
with **expanded occurrences**, not with unique subtrees.

We need:

1. An in-memory representation that shares identical denoted subterms.
2. Memoization that is sound under binders and shadowing.
3. Compatibility with the existing `Expr` API, diagnostics, TH, and (for now)
   legacy tree CBOR / semantic hashes.

---

## 2. Architectural split: surface vs semantic core

```text
  Parse / format / TH / optics / errors
                 │
                 ▼
            Expr s a          ← public, spans, Note, Import, …
                 │ denote / resolve
                 ▼
            CoreGraph a       ← hash-consed denoted syntax (NodeId arena)
                 │ NbE
                 ▼
            Val a             ← runtime values (ValId / EnvId)
                 │ quoteGraph
                 ▼
            CoreGraph a       ← shared normal form
                 │
        ┌────────┴────────┐
        ▼                 ▼
  toSharedExpr      encodeGraph (experimental)
  (physically       DAG CBOR — not default hash
   shared Expr)
        │
        ▼
  hashExpression / freeze / CLI  ← still legacy tree CBOR
```

**Keep `Expr`.** Changing its constructors would force an ecosystem-wide rewrite
(Template Haskell, `Data`/`Generic`, optics, LSP, pattern-matching clients).
The semantic engine gains a DAG; the surface stays a tree.

**Add `CoreGraph`.** Annotation-free denoted syntax in an append-only arena.
Child fields are `NodeId`s, not recursive Haskell pointers inside the payload.

---

## 3. Hash-consing (`CoreGraph`)

### 3.1 What it is

Whenever the builder would allocate a node that is structurally identical to an
existing one, it reuses the existing `NodeId`. After construction, equal
structure and equal identity coincide for interned nodes.

### 3.2 Contract

| Property | Choice |
|----------|--------|
| Storage | Dense `NodeId` (`Word32`) into a frozen `Vector` of `CoreNode` |
| Intern key | Exact **denoted** payload (no `Src`/`Note`; cosmetics as in `denote`) |
| Hash role | Non-cryptographic table index only; shallow `Eq` decides merges |
| Builder | `ST`-scoped mutable table + arena; freeze drops the table |
| ID scope | Meaningful only inside the owning graph; merge = reintern |
| Sharing mode | `SharingOn` / `SharingOff` (oracle: same `toSharedExpr`) |

Alpha-equivalence is **not** an intern key. `alphaNormalizeGraph` is an
explicit transformation. Open terms may share syntax under different binders;
evaluation interprets free variables relative to the ambient environment.

### 3.3 API surface (modules)

- `Dhall.Core.DAG` / `Dhall.Core.DAG.Node` — graph, builder, metrics, shift /
  subst / alpha, `toSharedExpr`, `prettyGraph`
- `Dhall.Core` re-exports: `normalizeGraph`, `fromExpr`, `toSharedExpr`,
  `graphMetrics`, `hashExpressionGraph` (legacy tree bytes of shared Expr),
  experimental `encodeGraph` / `decodeGraph`
- `Dhall.Core.CSE` — opt-in let-factoring for compact text (not β-NF, not hash)

### 3.4 Physical sharing vs algorithmic sharing

`toSharedExpr` rebuilds an `Expr` while memoizing `NodeId → Expr`, so the
Haskell heap reuses child pointers (`O(U)` unique nodes). Naive
`Foldable`/`Eq` walks that do not memoize still revisit every occurrence.

Large-NF tools should prefer graph APIs; casual pattern-matching keeps working
via adapters.

---

## 4. Normalization by evaluation

### 4.1 Flow

1. `denote` the input.
2. `eval` into `Val` under an `Environment`.
3. `quoteGraph` into a `Builder`, then `freeze` → `CoreGraph`.
4. Compatibility `normalize` = `toSharedExpr` of that graph.

`normalizeGraph` / `normalizeGraphWith` expose the graph directly.
`SharingOff` remains an oracle for byte/tree parity.

### 4.2 Identified values (`Val` / `Environment`)

Each `Val` carries:

- `valId` — monotonic `ValId` from the ambient eval run
- `valClosed` — whether the value is closed (gates quote/conv memos)
- payload — first-order data; closures hold binder, env, and body `NodeId`

`Environment` frames carry `EnvId`. Cons-list shape is preserved so closures
share tails cheaply.

Pattern synonyms preserve the old constructor-style API for `TypeCheck` and
callers (`VLam`, `VHPi`, `VPiND`, …). Non-dependent Pis from eval use `VPiND`
when possible; higher-order `VHLam`/`VHPi` remain for builtins and some
typechecker-synthesized Pis.

**Ids never appear** in `Show`, `Eq` of public terms, diagnostics, hashes, or
CBOR. Counter reset across runs must not collide with CAF builtin values:
`withEvalRun` clears caches/builder but **preserves** process-wide
`ValId`/`EnvId` counters.

### 4.3 Eval-run state (`Dhall.Eval.Run`)

One global run (IORef), reset at public entry points via `withEvalRun`:

| Piece | Role |
|-------|------|
| `ValId` / `EnvId` counters | Identity for memos |
| Intern builder | Bodies for closure/`infer` keys |
| Eval memo | `(body NodeId, EnvId, binder, arg ValId) → Val` |
| Quote memo | closed `ValId → NodeId` |
| Conv memo | closed `(ValId, ValId) → Bool` |
| Infer memo | `(NodeId, EnvId, typesId) → Val` |
| Caps / stats | Bound insertions; hit/miss counters |

**Default: speed caches off** (`defaultEvalConfig`). Opt in with
`cacheOnConfig`. Under `-threaded` Template Haskell, re-entrant
`unsafePerformIO` + `atomicModifyIORef'` memos previously blackhole-deadlocked
the RTS; forcing values under the lock was unsafe. Inserts now evaluate
*before* taking the lock, but caches stay off by default until a proven
non-reentrant design is measured.

`cacheOffConfig` is the oracle: zero insertions.

### 4.4 Conversion

`convImpl`:

1. Same `ValId` → equal (positive fast path).
2. Both closed → closed-pair memo (when caches on).
3. Else structural walk (binders via fresh vars / skip).

---

## 5. Typechecking

`typeWithA` runs inside one `withEvalRun`. Inference retains `Val`s in an
extended context (`Ctx`: value env + type env).

`TypesBind` carries an `EnvId` (`typesId`). Infer memo key:

```text
InferKey (denoted NodeId, value EnvId, types EnvId)
```

A node-only cache would be unsound for open terms. The typer is constant for a
given `typeWithA` run.

Diagnostics still use occurrence `Note`/`Src` on the surface expression, not a
global `NodeId → Src` map. Quotes for error messages go through `quoteInRun`.

---

## 6. CSE (presentation only)

`Dhall.Core.CSE` factors repeated structure into `let` bindings for compact
human-readable text.

- Candidates keyed by `(NodeId, capture environment)`, not `NodeId` alone.
- Must not merge shadowed open terms.
- Result is judgmentally equal / same type / same normalize, but **not** β-NF.
- Never used for `normalize`, `hashExpression`, or canonical CBOR.

---

## 7. Consumers and hashing

| Path | Behavior |
|------|----------|
| `normalize` | Graph → physically shared `Expr` |
| `hashExpression` | SHA-256 of **legacy tree** CBOR |
| `hashExpressionGraph` | Same, via `toSharedExpr` |
| `dhall hash` | `normalizeGraph` → `alphaNormalizeGraph` → shared Expr → hash |
| `estimateGraphSize` | Unique-node count from `graphMetrics` |
| Marshal decode | Still uses `normalize`; comment notes physical sharing |

Default integrity hashes are **unchanged**. Experimental DAG CBOR is separate.

---

## 8. Experimental DAG CBOR

Envelope: `["Dhall-DAG", 1, nodes, root]`.

- Re-intern with sharing on; number nodes by DFS postorder.
- Child edges as `Var "_" n` referring to `nodes[n]`.
- Child-before-parent; node-count bound; reject trailing data / bad versions.
- Module: `Dhall.Core.DAG.CBOR` (`encodeGraph` / `decodeGraph` /
  `hashGraphLegacy`).

Not the default semantic hash. See ADR 003. Future default switch is ADR 004
(versioned cache namespace, refreeze, coordinated standard bump)—**not done**.

---

## 9. Shadowing and memo soundness

Example:

```dhall
let x = 1
let y = f x
let x = 2
in  f x
```

| Layer | Rule |
|-------|------|
| Syntax DAG | Identical open syntax may share one `NodeId` |
| Eval / infer memos | Key includes `EnvId` / `typesId` |
| CSE | Key includes capture env; do not merge across different `x` binders |
| Closed duplicates | Safe to share, CSE at root, memo by id |

---

## 10. What we deliberately did not do

- Replace public `Expr` constructors.
- Hash-cons every `Val` into a second full term arena (RSS risk).
- Flip default CBOR / semantic hashes.
- Treat CSE output as normative NF.
- Use interner hashes as integrity hashes.
- Reset `ValId` counters on every `withEvalRun` (collides with CAF builtins).
- Enable speed caches by default under threaded TH.

---

## 11. Module map

| Module | Responsibility |
|--------|----------------|
| `Dhall.Core.DAG.Node` | `NodeId`, `CoreNode`, `childIds` |
| `Dhall.Core.DAG` | Builder, freeze, convert, graph ops, metrics |
| `Dhall.Core.DAG.CBOR` | Experimental DAG encode/decode |
| `Dhall.Core.CSE` | Opt-in let CSE |
| `Dhall.Eval` | NbE: `Val`, env, eval/quote/conv/normalizeGraph |
| `Dhall.Eval.Run` | Ids, caches, `withEvalRun`, stats |
| `Dhall.TypeCheck` | Infer with context-complete memo |
| `Dhall.Core` | Public re-exports |
| `Dhall.Import` | `hashExpression` / `hashExpressionGraph` / graph size |

---

## 12. Testing oracles

From the test matrix (ADR 002):

1. **Tree engine** — existing normalize / typecheck / hash goldens.
2. **Sharing-off** — same builder with intern lookup disabled; denoted/`toSharedExpr` parity.
3. **Cache-off** — zero memo insertions; output identical.
4. **Ids unobservable** — `Show` of normalized terms must not mention `ValId`/`EnvId`.
5. **DAG CBOR** — sharing on/off same bytes; round-trip denoted tree; independent of intern order; legacy graph hash matches tree hash.

---

## 13. Migration posture

Today: engine may use graphs and identified values; **on-disk and `sha256:`
remain legacy tree CBOR**.

When flipping (future release): bump language standard, new cache directory,
refreeze imports, update all hash goldens together, cross-check with an
independent codec. Until then, experimental DAG CBOR is for measurement and
tooling only.
