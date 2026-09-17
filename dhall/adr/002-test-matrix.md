# Shared core invariant and test matrix

This matrix is the Phase 0 gate: every later phase must keep these checks
green (or replace them only when a phase *intentionally* changes CBOR).

## Oracles

| Oracle | What it is | Used until |
|--------|------------|------------|
| Tree engine | Current `normalize` / `alphaNormalize` / `shift` / `subst` / `denote` on `Expr` | Phases 1–8 for semantics |
| Sharing-off | Same `CoreGraph` builder with intern lookup disabled | Phases 1–6 output parity |

Sharing on/off and tree/graph conversions compare **denoted** `Expr` values
(and, until Phase 8, legacy tree CBOR bytes).

## Phase 1 — graph representation

| Invariant | Test |
|-----------|------|
| Tree/graph round trip | `toSharedExpr (fromExpr e) == denote e` |
| Sharing on/off | `toSharedExpr (fromExprOn e) == toSharedExpr (fromExprOff e)` |
| Child-before-parent | Every child `NodeId` is strictly smaller than its parent |
| Acyclicity | Follows from child-before-parent |
| Bounds | Every stored id is `< length nodes`; root is in range |
| Compact | Compaction drops unreachable nodes; remaining ids remapped densely |
| Hash-collision safety | Distinct shallow keys stay distinct nodes even if hashes collide |
| Repeated tree size | Interning `t = NaturalPlus t t` depth `n` with sharing on uses `n+1` unique nodes, not `O(2^n)` |
| Sharing-off size | Same fixture uses `2^(n+1)-1` unique nodes |
| Shadowed binders (syntax) | Identical open syntax may share a `NodeId`; evaluation still depends on env (CSE must not merge — Phase 3) |
| Alpha/shift/subst parity | Graph ops match `Expr` ops on denoted trees |

## Later phases (recorded now, implemented later)

| Phase | Extra invariants |
|-------|------------------|
| 2 | `normalize` via graph equals tree `normalize`; quote does not expand unique nodes |
| 3 | CSE is opt-in; `judgmentallyEqual e (cse e)`; shadowed binders not merged |
| 4 | Val/Env ids unobservable in `Show`; cache-off inserts nothing; sharing on/off byte-identical; same `ValId` quotes to the same node |
| 5 | Typecheck verdicts match; diagnostics keep occurrence `Src`; infer memo keys `(NodeId, EnvId, typesId)` |
| 6 | Public `CoreGraph` normalize/hash/pretty/CSE; `hashExpressionGraph` is legacy tree bytes of `toSharedExpr` |
| 7 | Experimental DAG CBOR is deterministic and independent of intern order; default stays tree CBOR |
| 8 | Default tree CBOR/hashes unchanged; cache namespace and refreeze documented for a future switch |
