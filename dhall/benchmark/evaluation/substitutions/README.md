# substitutions — many Haskell-API substitutions

This fixture measures import resolve when `InputSettings` carry a large
substitution map, which none of the other evaluation groups do. That is the
customer `inputExprWithSettings` shape: injected type names plus an imported
AST.

There are **two** probes. They measure different things; do not treat the
nested-let group as a stand-in for the customer as-code regression.

## 1. Nested lets — identity fast path (`substitutions.as_code` / `.as_source`)

One imported file, 8000 nested `let xᵢ = 0`. The 100 `UserType000`–`UserType099`
keys are **intentionally disjoint** from those binders, and substitution
values are closed (`λ(a : Type) → {…}`). `substitute()` runs about twice.

This is the *best* case for `shiftSubstitutions` returning the same map
pointer. It will **not** show the customer as-code slowdown (hundreds of
Code imports each rebuilding `ResolvedSubstitutions`).

| File | Role |
|------|------|
| `package.dhall` | 8000 nested `let xᵢ = 0 in 0` (regenerate with `python3 generate.py`) |
| `pipeline-code.dhall` | `./package.dhall` |
| `pipeline-source.dhall` | `./package.dhall as Source` |

## 2. Many files — as-code regression (`substitutions.many_files.*`)

200 imported modules, each binding `a` and `x`, plus 200 Haskell-API keys
whose values contain a **free** `a`. Matches the profiled job: `substitute()`
once per Code file, and `shiftSubstitutions` cannot no-op at `let a`.

The modules are generated into a temp directory during bench prep (not
checked in). A single large file would **not** work: Code would call
`substitute` only a couple of times, which is what the nested-let group
already measures.

See `many_files/README.md`.

## 3. Shift cost — plan (1)+(2) (`substitutions.shift_cost.*`)

The Mode B groups above **do not** move with per-value shift or root-shift
memo:

- nested-let is already an identity path (no shift work)
- `many_files` is dominated by parse/typecheck/normalize of 200 files
  (~100 ms); shift is cheaper than tasty-bench noise even after fattening
  the map

`substitutions.shift_cost` is a **pure** `nf` probe: the same fat
`manyCollidingSubstitutions` map and `let a` / `let x` module shape, with
no import I/O. The map is resolved **once**. `naive` is an in-harness copy
of `substituteManyNaive` (`Map.map shift` on every value at a non-key
binder, no root-shift memo). `optimized` is an in-harness copy of
`substituteManyFromRoot` (per-value shift + root-shift memo across the 200
expressions). Both copies live in the harness so this group compiles before
those helpers exist in `Dhall.Substitution`.

## Harness

The four import groups are **Mode B**: parse-only prep; each sample uses a
fresh `XDG_CACHE_HOME` and `Dhall.resolveWithSettings` (the library path
that applies substitutions, not `Import.loadWithStatus` alone).
`shift_cost` is not Mode B — it never touches the importer.

| Group | Benchmark |
|-------|-----------|
| `substitutions.as_code` | `resolve_cold_cache_on` |
| `substitutions.as_source` | `resolve_cold_cache_on` |
| `substitutions.many_files.as_code` | `resolve_cold_cache_on` |
| `substitutions.many_files.as_source` | `resolve_cold_cache_on` |
| `substitutions.shift_cost.naive` | pure `nf` (in-harness `substituteManyNaive`) |
| `substitutions.shift_cost.optimized` | pure `nf` (in-harness `substituteManyFromRoot`) |

```sh
stack bench evaluation --ba '--pattern substitutions'
stack bench evaluation --ba '--pattern substitutions.many_files'
stack bench evaluation --ba '--pattern substitutions.shift_cost'
```

CLI wall-clock of the same four pipelines, **without** Haskell substitutions,
from this directory (`./run.sh`):

- nested-let `pipeline-code.dhall` / `pipeline-source.dhall`
- many-files tree generated into a temp directory (not checked in), then
  the same Code / `as Source` pipelines

Each `run.sh` sample uses a fresh `XDG_CACHE_HOME`. That is a Mode B
analogue for import load, but it is still a full `dhall --file` evaluate,
not `resolveWithSettings`.

`ResolvedSubstitutions` is computed once per run and stored on import
`Status` (not on public `EvaluateSettings`). `shiftSubstitutions` identity
/ `Map.map` fast path is gated by `substitutionOptimizationsEnabled` in
`dhall/src/Dhall/Substitution.hs`.

## What to read

- **`as_source` (nested lets)**: substitution walks the unnormalized
  nested-let AST during `finalizeSourceImport`. Dominated by map rebuilds
  at every binder unless the identity fast path fires.
- **`as_code` (nested lets)**: the same map is applied once on the
  pre-normalize tree, then beta-normalize collapses the lets to `0`.
- **`many_files.as_code`**: this is the group that should move with the
  customer as-code regression (18s → 30s was `resolveSubstitutions` +
  `freeVarNames` once per imported file). It does **not** isolate (1)+(2).
- **`shift_cost`**: this is the group that should move with plan (1)+(2).
  Compare `naive` vs `optimized`; ignore wall-clock vs Mode B rows.

Do not compare these numbers to Mode A `resolve` rows (those use
`loadWithStatus` without the library substitution wrapper).
