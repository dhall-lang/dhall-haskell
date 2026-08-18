# Evaluation benchmark suite

`stack bench evaluation` measures Dhall parse / resolve / typecheck / evaluation
phases on a mix of synthetic and real-world fixtures. The harness lives in
`Main.hs`.

This README explains **how to read the numbers**. Fixture-specific notes live
in each subdirectory (`large3/`, `large6/`, etc.).

## Running

```sh
cd dhall && stack bench evaluation
stack bench evaluation --ba '--pattern large6'
stack bench evaluation --ba '--pattern prelude_import'
stack bench evaluation --ba '--pattern semisemantic.nf_size_walk'
stack bench evaluation --ba '--list-tests --pattern large3'
```

Pattern matching is infix (tasty default): `--pattern slow_walk` matches
`large6.slow_walk.as_code.resolve`.

## CLI timings (`run.sh`)

Each fixture directory has a `run.sh` that times a plain `dhall --file …`
(or, for `k8s/`, `dhall` on the `.mkPod` expression). Run it from that
subdirectory. This is a full CLI evaluate (parse + resolve + typecheck +
normalize), not a Mode A phase split.

```sh
cd dhall/benchmark/evaluation/large5 && ./run.sh
```

`substitutions/run.sh` times the nested-let pipelines and a temp-generated
many-files tree (Code and `as Source`). It still cannot inject Haskell
`InputSettings` substitutions; use the tasty-bench group for that.

## Three measurement modes

The harness uses three different strategies. **Always check which mode a
benchmark group uses before comparing numbers.**

### Mode A — phase benchmarks (default)

Used by: `normalize/*`, `large1`, `k8s`, `large3*`, `large4`, `large5*`, and
most `large6` groups.

| Step | What happens |
|------|----------------|
| **Prep** | Parse; resolve with `UseSemanticCache` (semantic + semisemantic disk caches on); typecheck once to reject broken fixtures |
| **`resolve` bench** | Re-resolve parsed AST with `IgnoreSemanticCache` (semantic disk cache off; **semisemantic disk cache still on**) |
| **`typecheck` bench** | Type-check the **pre-resolved AST from prep** (not the re-resolved one) |
| **`evaluation` bench** | Normalize the **pre-resolved AST from prep** |

Implications:

- `resolve` is **not** a fully cold import load. Prep warms the semisemantic
  cache (`$XDG_CACHE_HOME/dhall-haskell/`), so Code imports that already paid
  typecheck+normalize during prep can look near-instant on re-resolve.
- `typecheck` / `evaluation` measure only that phase on an already-resolved
  tree. They do **not** include import-loading cost.
- In-memory per-run caches (`_cache`, `_parsedImportCache`, …) are fresh on
  each timed `resolve` iteration; only **disk** caches persist from prep.

This mode is good for **phase regression** and for fixtures where resolve still
shows meaningful work (parse-heavy trees, large3, Source typecheck paths).

### Mode B — `resolve_cold_cache_on`

Used by:

- `large6.slow_eval.as_code`
- `large6.slow_typecheck.as_code`
- `large6.slow_normalize.as_code`
- `large6.slow_multi.as_code`
- `prelude_import.code`
- `prelude_import.source`
- `substitutions.as_code`
- `substitutions.as_source`
- `substitutions.many_files.as_code`
- `substitutions.many_files.as_source`

| Step | What happens |
|------|----------------|
| **Prep** | Parse only — **no** cache-warming resolve |
| **`resolve_cold_cache_on` bench** | Resolve with caches **enabled**, under a **fresh `XDG_CACHE_HOME` per sample** |

This is the right number for **cold import load with normal caching behavior**
(first run in a clean cache directory, still writing semantic/semi-semantic
entries).

Mode A previously reported ~μs for the Code `large6` variants above because
prep had warmed semisemantic; those groups were moved to Mode B.

### Mode C — Source cost deferral (implicit)

Some `as Source` fixtures keep heavy work out of the resolved AST:

| Fixture | Slow work shows on |
|---------|-------------------|
| `large6.slow_eval.as_source` | **`evaluation`** (~0.5 s fold) |
| `large6.slow_normalize.as_source` | **`evaluation`** (~0.5 s normalize); `resolve` often ~1 ms (semisemantic hit) |
| `large6.slow_multi.as_source` | **`evaluation`** (~0.4 s); `resolve` often ~8 ms |
| `large6.slow_typecheck.as_source` | **`resolve` + `typecheck`** (~0.22 s each; assert stays in AST) |
| `large6.slow_parse.*` | **`resolve`** (~0.58 s; parse not semisemantic-cached) |
| `large6.slow_walk.*` | **`resolve`** (~0.53 s; structural walk) |

For Source normalize/multi, **do not** compare `resolve` to Mode B Code numbers;
read `evaluation` (or cold `dhall hash`).

## Fixture index

| Group | Directory | Modes | Purpose |
|-------|-----------|-------|---------|
| `normalize.*` | `normalize/` | A | NbE on small resolved terms |
| `large1` | `large1/` | A (+ parse bench) | Medium import tree |
| `large2` | `large2/` | prep only | CBOR encode/decode |
| `k8s.*` | `k8s/` | A | Real k8s schema imports |
| `large3` | `large3/` | A | Sourcegraph-scale Code pipeline (~193 MB NF) |
| `large3.source` | `large3/` | A | Same tree under `as Source` |
| `large3.get_config.*` | `large3/` | A | Small projection; still walks full graph |
| `large4` | `large4/` | A | Medium customization tree (Code; may OOM) |
| `large4.source` | `large4/` | A | Same tree with `apply-all.dhall as Source` |
| `large5.code` / `large5.source` | `large5/` | A | Small tree, Code vs Source |
| `large6.slow_*` | `large6/` | A, B, or C | Isolated ~0.5 s artificial burdens |
| `prelude_import.*` | `prelude_import/` | B | Full Prelude package, Code vs Source |
| `substitutions.*` | `substitutions/` | B | Nested-let identity-path probe (100 closed keys) |
| `substitutions.many_files.*` | generated at prep | B | 200 imports × 200 colliding Haskell-API keys (as-code regression) |
| `substitutions.shift_cost.*` | in-harness | pure `nf` | Naive `Map.map shift` walker vs `substituteManyFromRoot` |
| `semisemantic.nf_size_walk.*` | in-harness | pure `nf` | Full NF size walk vs early abort at 64KiB (shows the store-NF cutoff) |

See `large6/README.md` for the full per-variant matrix. See
`substitutions/README.md` for the substitution probe (uses
`Dhall.resolveWithSettings`, not `Import.loadWithStatus`).

## Disk caches vs CLI

| Cache | Directory | `resolve` bench | `resolve_cold_cache_on` |
|-------|-----------|-----------------|-------------------------|
| Semantic (integrity-hash products) | `dhall/` | Off (`IgnoreSemanticCache`) | On |
| Semisemantic v2 (merkle key → small NF or well-typed marker) | `dhall-haskell-v2/` | **On** (warmed by prep in Mode A) | On (fresh dir) |

Unhashed Code imports key the semisemantic cache by a **merkle hash** of the
local denoted AST with child edge hashes (plus starting-context /
substitution fingerprints), not by CBOR of the fully substituted tree. The
payload is either a small CBOR normal form (skip typecheck + normalize on hit)
or a well-typed marker (skip typecheck only; normalize the in-memory tree).
Large normal forms are never written. Legacy `dhall-haskell/` entries are
ignored.

`dhall --no-cache` disables only the semantic cache, not semisemantic. That is
why Mode A `resolve` can be much faster than a true cold CLI run (e.g. large3
~16 s bench vs ~58 s cold `dhall resolve`).

## Interpreting common surprises

| Observation | Likely cause |
|-------------|----------------|
| Code `large6` normalize/multi at ~μs | Fixed — use `resolve_cold_cache_on` rows |
| Source `large6` normalize/multi resolve ~1–8 ms | Semisemantic hit after prep; cost on **evaluation** |
| `large3.get_config.*` evaluation ~160 μs | Tiny NF after huge resolve |
| `large5.source` much faster than `large5.code` on resolve | Source skips building huge NF during import |
| `prelude_import.source` faster than `.code` | Prelude under `as Source` avoids Code normalize path |
| Early-abort NF size walk does not move import groups | The cutoff only runs on a **cold Code miss** of a **many-node** NF. Mode A resolve is cache-warm; substitutions NFs are tiny; a large `Text` payload is O(1) length. Use `semisemantic.nf_size_walk` (full vs early_abort). |
| Opportunistic `as Source` cache fill toggle | No effect on these fixtures (no matching `ImportAlt` pattern) |

## Related docs

- `large3/README.md` — Sourcegraph fixture, get_config CLI vs bench gap
- `large4/README.md` — customization tree
- `large5/README.md` — small Code vs Source tree
- `large6/README.md` — slow-child matrix and burden placement
- `prelude_import/README.md` — Prelude cold import
- `../../import-explanation.md` §10 — `as Source` performance work and bench notes
- [`.github/workflows/README.md`](../../../.github/workflows/README.md) — CI job, `gh-pages` charts, on-demand runs
