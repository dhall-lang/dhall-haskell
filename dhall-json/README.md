# `dhall-json 1.2.4`

[![Hackage](https://img.shields.io/hackage/v/dhall-json.svg)](https://hackage.haskell.org/package/dhall-json)

This `dhall-json` package provides a Dhall to JSON compiler and a Dhall to YAML
compiler.  The reason this package is called `dhall-json` is that the Haskell
`yaml` library uses the same data structure as Haskell's `aeson` library for
JSON

## Quick start

If you have Nix installed then you can build and run this package using:

```bash
$ nix-build
$ result/bin/dhall-to-json <<< "{ foo = 1, bar = True }"
{"foo":1,"bar":true}
$ result/bin/dhall-to-json <<< "List/head Integer ([] : List Integer)"
null
$ result/bin/dhall-to-yaml <<< "{ foo = [1, 2, 3] : List Integer, bar = { baz = True } }"
foo:
- 1
- 2
- 3
bar:
  baz: true
```
