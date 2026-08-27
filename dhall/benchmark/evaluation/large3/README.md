This example is taken from git@github.com:sourcegraph/deploy-sourcegraph-dhall-archived.git

The command `dhall --file pipeline.dhall` takes about 30 seconds. The text size of the normal form is about 193 MB.

See `../README.md` for how the evaluation harness measures resolve vs typecheck vs evaluation.

## Benchmark groups (Mode A — phase benchmarks)

| Group | Root expression | Notes |
|-------|-----------------|-------|
| `large3` | `pipeline.dhall` — `./package.dhall` then `Render` | Large normal form (~193 MB) |
| `large3.get_config` | `get_config.dhall` — `package.Configuration.Global::{=}` | Small final NF |

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

Bench resolve is **not** fully cold: prep warms semisemantic cache, so re-resolve
is faster than CLI with empty cache. 

## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern large3'
stack bench evaluation --ba '--pattern get_config'
stack exec -- dhall --file ./benchmark/evaluation/large3/get_config.dhall >/dev/null
```
