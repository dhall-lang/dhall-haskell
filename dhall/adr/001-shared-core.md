# ADR 001: Hash-consed semantic core (`CoreGraph`)

Status: accepted for the shared-core implementation (Phases 0–8 of the plan).

## Context

The public `Expr` type is a source-facing tree: each constructor owns its
children, so evaluation and quoting copy structure. Large normal forms then
scale with expanded occurrences rather than unique subtrees.

## Decision

Keep `Expr` as the source API (spans, formatting, TH, optics, downstream
pattern matching). Add an internal hash-consed `CoreGraph` for denoted
semantics:

- An append-only arena of `CoreNode` values, addressed by dense `NodeId`
  (`Word32`). Child fields are ids, not recursive Haskell pointers in the
  payload.
- Interning is **exact denoted syntax** (no `Note`/`Src`, cosmetics as in
  `denote`). Alpha-normalization is an explicit graph transformation, not an
  intern key.
- Build in `ST` with a mutable intern table; freeze to an immutable vector +
  root and drop the table. Ids are meaningful only inside their owning graph.
  Merging graphs means reinterning reachable nodes, never copying foreign ids.
- The intern hash is a table index only. Collisions may affect speed, never
  which nodes merge. Dhall semantic `sha256` remains a hash of encoded bytes.
- Sharing can be disabled: the same builder always appends, producing a
  tree-shaped graph. Sharing on/off must yield identical `toSharedExpr`
  results. Allocation ids never appear in output, diagnostics, or hashes.
- Memoized graph operations that are context-sensitive (shift, subst, alpha)
  include that context in the memo key. Open `NodeId` equality is syntactic,
  not judgmental equality under different environments.

## Consequences

- Quote/normalize can produce a compact DAG (Phase 2) without changing the
  public `Expr` constructors.
- CSE (Phase 3) must key candidates by `(NodeId, capture environment)`, not
  `NodeId` alone.
- Default CBOR/hash stay on the legacy tree encoding until the coordinated
  format migration (Phases 7–8).
- `toSharedExpr` reconstructs a physically shared Haskell `Expr` in `O(U)`
  unique nodes. Naive `Foldable` walks remain occurrence-sensitive.
