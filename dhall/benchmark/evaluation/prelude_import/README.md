# Prelude import benchmark

Measures **cold** import loading of the full Dhall Prelude from the vendored
`dhall-lang/Prelude` submodule.

See `../README.md` — this fixture uses **Mode B** (`resolve_cold_cache_on`).

## Fixtures

| File | Imports |
|------|---------|
| `prelude-code.dhall` | `../../../dhall-lang/Prelude/package.dhall` |

## Benchmark groups

| Group | Benchmark |
|-------|-----------|
| `prelude_import.code` | `resolve_cold_cache_on` |

Each sample:

- semantic and semisemantic caches **enabled**
- fresh `XDG_CACHE_HOME` (true cold per iteration)

Prep only parses; no cache-warming resolve.

Typical full-suite numbers: Code ~380 ms, Source ~230 ms.

## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern prelude_import'
```
