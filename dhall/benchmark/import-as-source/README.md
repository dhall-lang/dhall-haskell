# import-as-source micro-benchmark

Separate from `stack bench evaluation`. Compares Code vs `as Source` on small
fixtures with a fresh `XDG_CACHE_HOME` per cold-resolve sample.

## Running

```sh
cd dhall && stack bench import-as-source
```

## Fixtures

Under `fixtures/`:

- `importers/*-code.dhall` / `*-source.dhall` — same logic, different import mode
- `roots/` — entry points for typecheck benches
- `common/` — shared slow children (`long-eval`, `long-typecheck`)

## Benchmark groups

Per fixture name (`assert`, `field`, `typecheck`):

| Phase | What it measures |
|-------|------------------|
| `resolve-cold.code` / `.source` | Cold resolve with caches on, fresh cache dir |
| `typecheck-resolved.code` / `.source` | Type-check pre-resolved AST from load |

Unlike `evaluation` Mode A, this suite does not warm semisemantic during prep
for the cold resolve benches (each resolve sample gets a new cache directory).

See `benchmark/evaluation/README.md` for the larger fixture suite and harness
modes used by `stack bench evaluation`.
