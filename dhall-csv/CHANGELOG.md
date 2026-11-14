1.0.5

* `codeToValue` and `resolveSchemaExpr` take a `RemoteImportMode`.
  `dhall-to-csv` and `csv-to-dhall` accept `--no-remote`, which rejects http
  and https imports used as code, as Text, or as Bytes, including
  integrity-checked cache hits.

1.0.4

* Build against `dhall-1.42.0`

1.0.3

* Builds against newer dependencies
  * [`vector-0.13`](https://github.com/dhall-lang/dhall-haskell/pull/2427)

1.0.2

* Build against `dhall-1.41`
* [Build against `text-2.0`](https://github.com/dhall-lang/dhall-haskell/pull/2356)
1.0.1

* [Build against `tasty-silver` 3.3](https://github.com/dhall-lang/dhall-haskell/pull/2307)

1.0.0

* Initial release
