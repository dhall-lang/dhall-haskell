This example is taken from git@github.com:sourcegraph/deploy-sourcegraph-dhall-archived.git

The command `dhall --file pipeline.dhall` takes about 30 seconds. The text size of the normal form is about 193 MB.

See `../README.md` for how the evaluation harness measures resolve vs typecheck vs evaluation.

## Benchmark groups (Mode A — phase benchmarks)

| Group | Root expression | Notes |
|-------|-----------------|-------|
| `large3` | `pipeline.dhall` — `./package.dhall` then `Render` | Large normal form (~193 MB) |
| `large3.source` | `pipeline-source.dhall` — same with `as Source` | Same tree under `as Source` |
| `large3.get_config.code` | `get_config.dhall` — `package.Configuration.Global::{=}` | Small final NF; ~17s CLI |
| `large3.get_config.source` | `get_config_as_source.dhall` — same with `as Source` | Small final NF; slower than Code (~30s CLI) |

Each group reports `resolve`, `typecheck`, and `evaluation`:

- **Prep** resolves with disk caches on (warms semisemantic v2).
- **`resolve`** re-resolves with semantic cache off; semisemantic v2 still helps.
- **`typecheck` / `evaluation`** run on the pre-resolved AST from prep.

`get_config*` only needs configuration defaults, but `./package.dhall` still
imports `Render`, so import resolution walks the full package graph either way.
`get_config.*.evaluation` is ~160 μs because the projected NF is tiny after
the expensive resolve.

Semisemantic entries live under `~/.cache/dhall-haskell-v2/` (merkle key →
small NF or well-typed marker). Giant NFs are not written.

## Observed timings

| Group | Bench resolve | Bench typecheck | Bench evaluation | Cold `dhall resolve` |
|-------|---------------|-----------------|------------------|----------------------|
| `large3` (Code) | ~16.1 s | ~2.3 s | ~0.8 s | ~58 s |
| `large3.source` | ~10.3 s | ~3.1 s | ~0.5 s | ~24 s |
| `large3.get_config.code` | ~16.2 s | ~2.3 s | ~160 μs | (same resolve path) |
| `large3.get_config.source` | ~11.0 s | ~3.0 s | ~167 μs | (same resolve path) |

Bench resolve is **not** fully cold: prep warms semisemantic cache, so re-resolve
is faster than CLI with empty cache. The gap is largest on plain Code (~16 s vs
~58 s).

## CLI vs bench

| Mode | CLI full (`dhall --file`) | Bench resolve |
|------|---------------------------|---------------|
| Code (`get_config.dhall`) | ~17–20 s | ~16.2 s |
| `as Source` (`get_config_as_source.dhall`) | ~30–37 s | ~11.0 s |

End-to-end CLI is slower for `as Source` on `get_config` even though bench
resolve looks faster for Source. Code normalizes during import; Source builds a
large non-normalized package and pays typecheck/eval later. See
`import-explanation.md` §10.7.

## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern large3'
stack bench evaluation --ba '--pattern get_config'
stack exec -- dhall --file ./benchmark/evaluation/large3/get_config.dhall >/dev/null
stack exec -- dhall --file ./benchmark/evaluation/large3/get_config_as_source.dhall >/dev/null
```
