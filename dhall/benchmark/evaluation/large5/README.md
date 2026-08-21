This benchmark isolates the extra import-resolution work done by `as Source`
on a wide local import tree.

See `../README.md` for harness measurement modes.

## Structure

- `pipeline-code.dhall` imports `./package.dhall` in normal `Code` mode.
- `pipeline-source.dhall` imports `./package.dhall as Source`.
- `package.dhall` exports a small configuration record and a `Render` function.
- `src/base/render.dhall` concatenates the outputs of four local generators.
- Each generator imports `shared/payload.dhall`, which aliases the large record
  in `../large4/base/record.dhall`.
- The final value is a list of 128 wrapper records, each embedding that large
  payload.
- The printed normal form is about 20.5 MB.

Conceptually similar to `large3` (package + render fan-out) but much smaller:
no k8s schemas, one shared payload, hand-inspectable tree.

## Benchmark groups (Mode A)

| Group | Benchmarks |
|-------|------------|
| `large5.code` | resolve, typecheck, evaluation |
| `large5.source` | resolve, typecheck, evaluation |

Typical full-suite numbers:

| Group | resolve | typecheck | evaluation |
|-------|---------|-----------|------------|
| `large5.code` | ~1.18 s | ~455 ms | ~134 ms |
| `large5.source` | ~177 ms | ~45 ms | ~51 ms |

Source wins sharply on **resolve** because it avoids building the ~20 MB normal
form during import loading. Typecheck/evaluation are also lower on the
Source-shaped resolved AST.

## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern large5'
stack exec -- dhall --file ./benchmark/evaluation/large5/pipeline-code.dhall >/dev/null
stack exec -- dhall --file ./benchmark/evaluation/large5/pipeline-source.dhall >/dev/null
stack exec -- dhall hash --file ./benchmark/evaluation/large5/pipeline-code.dhall
stack exec -- dhall hash --file ./benchmark/evaluation/large5/pipeline-source.dhall
```

`dhall resolve` on an `as Source` root measures source-artifact construction,
not the same path as Code to a normalized value — prefer the bench groups above
for Code vs Source comparison.
