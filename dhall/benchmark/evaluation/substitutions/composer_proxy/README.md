# composer_proxy — synthetic substitution-heavy end-to-end bench

Generated fixtures for a substitution-heavy import load: wide-record ASTs,
a large Haskell-API substitution map, then resolve → typecheck → normalize
via `Dhall.resolveWithSettings`.

“Wide” here means **many `Dhall.Map` nodes in the expression tree**, not long
source text or a heavy beta-reduction. Each module is short to parse; cost
shows up when walking/substituting/typechecking/normalizing the large record
type and value. The normal form is correspondingly large in **node count**
(same shape as the value), not because of deep computation.

There are **two** generated trees. The original flat list is a control; the
`many_imports` graph is the customer-shaped probe.

## 1. Flat list (`substitutions.composer_proxy.*`)

| Signal | Choice |
|--------|--------|
| Many Code imports | 400 generated modules, each imported once |
| Large `InputSettings` substitution map | 400 closed `UserType*` record **types** (64 fields each) |
| Wide AST | 64-field Natural **records** under `λ(_ : UserType000) → …` |
| Library substitution path | `Dhall.resolveWithSettings` |
| Cache | Mode **D** cold only (`end_to_end_cold`) |
| Code vs Source | `pipeline-code.dhall` / `pipeline-source.dhall` |

This group does **not** reproduce the customer as-source slowdown: every
module is imported once, so Source finalization does not re-walk shared
subtrees, and there are no hash-protected children.

## 2. Overlapping graph (`substitutions.composer_proxy.many_imports.*`)

Generated at prep (not committed):

| Layer | Shape |
|-------|--------|
| `leaves/` | 400 closed 64-field Natural records (hashed) |
| `parents/` | 80 modules; each imports 40 overlapping hashed leaves, annotated with `UserType*` |
| `aggregators/` | 16 modules; each imports 20 overlapping unhashed parents |
| `package.dhall` | list of aggregators |

Overlapping **unhashed** parents are the Source-cost amplifier: Code hits
`_cache` / `dhall-haskell-v2/` on repeat; Source currently re-walks parent
artifacts. Hashed leaves exercise Source preserve/inline of hash-protected
children.

| Group | Benchmark | Mode |
|-------|-----------|------|
| `substitutions.composer_proxy.many_imports.as_code` | `cold`, `warm` | D + E |
| `substitutions.composer_proxy.many_imports.as_source` | `cold`, `warm` | D + E |

`warm` prep-populates a dedicated temp `XDG_CACHE_HOME` and reuses it.

## Relation to other substitution probes

- Prefer `substitutions.many_files` for resolve-only cost of many **small**
  imports (`let a = i let x = 1 in a + x`) with a colliding substitution map.
- Prefer the flat `composer_proxy` group when the interesting cost is the
  combined import + typecheck + normalize path on **wide-record** ASTs with
  no sharing.
- Prefer `many_imports` when comparing Code vs Source under a shared,
  hashed, overlapping import graph (customer-shaped).

## Harness

Generated at prep into a temp directory (not committed).

```sh
stack bench evaluation --ba '--pattern substitutions.composer_proxy'
stack bench evaluation --ba '--pattern substitutions.composer_proxy.many_imports'
```
