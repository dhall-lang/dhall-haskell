This benchmark isolates the extra import-resolution work
on a wide local import tree.

See `../README.md` for harness measurement modes.

## Structure

- `pipeline-code.dhall` imports `./package.dhall` in normal `Code` mode.
- `package.dhall` exports a small configuration record and a `Render` function.
- `src/base/render.dhall` concatenates the outputs of four local generators.
- Each generator imports `shared/payload.dhall`, which aliases the large record
  in `../large4/base/record.dhall`.
- The final value is a list of 128 wrapper records, each embedding that large
  payload.
- The printed normal form is about 20.5 MB.

Conceptually similar to `large3` (package + render fan-out) but much smaller:
no k8s schemas, one shared payload, hand-inspectable tree.


## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern large5'
stack exec -- dhall --file ./benchmark/evaluation/large5/pipeline-code.dhall >/dev/null
stack exec -- dhall hash --file ./benchmark/evaluation/large5/pipeline-code.dhall
```

