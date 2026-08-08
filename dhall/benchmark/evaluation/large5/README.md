This benchmark isolates the extra import-resolution work done by `as Source`
on a wide local import tree.

Structure:

- `pipeline-code.dhall` imports `./package.dhall` in normal `Code` mode.
- `pipeline-source.dhall` imports `./package.dhall as Source`.
- `package.dhall` exports a small configuration record and a `Render` function.
- `src/base/render.dhall` concatenates the outputs of four local generators.
- Each generator imports `shared/payload.dhall`, which aliases the large record
  in `../large4/base/record.dhall`.
- The final value is a list of 128 wrapper records, each embedding that large
  payload.
- The printed normal form is about 20.5 MB.

This is conceptually similar to `large3` because:

- the root file has one package import
- the package re-exports configuration plus a render function
- the render function fans out into several local generators
- the final import-free value is large because many generated items embed a
  shared structured payload

It is much simpler than `large3` because:

- there are no Kubernetes schema imports
- there is no Sourcegraph-specific logic
- there is only one shared large payload
- the local import tree is small enough to inspect by hand

Suggested commands:

```sh
cd dhall && stack bench evaluation --ba '--pattern large5'
stack exec -- dhall --file ./dhall/benchmark/evaluation/large5/pipeline-code.dhall > /dev/null
stack exec -- dhall --file ./dhall/benchmark/evaluation/large5/pipeline-source.dhall > /dev/null
```

For semantic-hash comparison:

```sh
stack exec -- dhall hash --file ./dhall/benchmark/evaluation/large5/pipeline-code.dhall
stack exec -- dhall hash --file ./dhall/benchmark/evaluation/large5/pipeline-source.dhall
```

`dhall resolve` is still useful, but it is not the best apples-to-apples
comparison here: on an `as Source` root it primarily measures construction of
the source-preserving artifact, not the full path to the same final normalized
value.
