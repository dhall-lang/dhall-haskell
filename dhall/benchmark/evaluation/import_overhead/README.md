# import_overhead

Fixtures for the cost of data retained during import resolution: the shared
import twin, import alternatives, root substitutions, a large starting
context, and lazy versus forced import bodies.

Generated trees are built during prep and are not committed. The projection
pair under `projection/` is committed so the CLI timings have a stable path.

Run one group at a time, from `dhall/`:

```sh
stack bench dhall:evaluation --ba '--pattern import_overhead.custom_normalizer.cold'
stack bench dhall:evaluation --ba '--pattern import_overhead.custom_normalizer.warm'
stack bench dhall:evaluation --ba '--pattern import_overhead.import_alt.cold'
stack bench dhall:evaluation --ba '--pattern import_overhead.import_alt.warm'
stack bench dhall:evaluation --ba '--pattern import_overhead.import_alt.typecheck'
stack bench dhall:evaluation --ba '--pattern import_overhead.root_substitutions'
stack bench dhall:evaluation --ba '--pattern import_overhead.starting_context.cold'
stack bench dhall:evaluation --ba '--pattern import_overhead.starting_context.warm'
stack bench dhall:evaluation --ba '--pattern import_overhead.projection'
stack bench dhall:evaluation --ba '--pattern import_overhead.force_all'
```

`import_overhead.import_alt.typecheck` is a separate bench from `.cold`. The
pattern `import_overhead.import_alt.cold` does not select it.

## Groups

| Bench | What it times |
| --- | --- |
| `import_overhead.custom_normalizer.cold.default` / `.warm.default` | 700 one-line files exporting `0`, each imported twice. Default normalizer (shared twin). |
| `import_overhead.custom_normalizer.cold.custom` / `.warm.custom` | Same tree. Normalizer rewrites `NaturalLit 0` to `NaturalLit 7`, so normalization walks the inlined tree. Separate warm cache from `.default`. |
| `import_overhead.import_alt.cold` / `.warm` | 300 files of `missing sha256:… ? ./real.dhall`. The hash is the semantic hash of `0`, so resolution succeeds and a cold run fills the semantic cache from the fallback. |
| `import_overhead.import_alt.typecheck` | Typecheck only, on a tree resolved during prep. |
| `import_overhead.root_substitutions.resolve` | `resolveWithSettings` on one expression and 500 closed substitution keys, one of which is used. No imports. |
| `import_overhead.starting_context.cold` / `.warm` | 100 starting-context binders (`ctx000` … `ctx099 : Natural`) and 370 imported files that mention `ctx000`. |
| `import_overhead.projection.end_to_end_cold` | 200 imported 8-field records. The root names every import and evaluates one field (`Natural/show rs.r000.f00`). |
| `import_overhead.force_all.end_to_end_cold` | The same 200 records, evaluated as a record of every import. |

Cold benches use a fresh `XDG_CACHE_HOME` per sample. Warm benches reuse one cache directory filled by a single prep run.

## CLI (not a tasty group)

Same projection file, from `dhall/`. These are one-shot `+RTS -s` timings, not tasty-bench groups:

```sh
stack exec -- dhall +RTS -s --RTS --file benchmark/evaluation/import_overhead/projection/projection.dhall
stack exec -- dhall +RTS -s --RTS resolve --file benchmark/evaluation/import_overhead/projection/projection.dhall
stack exec -- dhall +RTS -s --RTS type --file benchmark/evaluation/import_overhead/projection/projection.dhall
stack exec -- dhall +RTS -s --RTS text --file benchmark/evaluation/import_overhead/projection/projection.dhall
```

`dhall text` is valid because `projection.dhall` evaluates to `Text`.
