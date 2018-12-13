# `dhall-json 1.2.5`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

Full documentation here:

* [`dhall-json` instructions](https://hackage.haskell.org/package/dhall-json/docs/Dhall-JSON.html)

## Introduction

This `dhall-json` package provides a Dhall to JSON compiler and a Dhall to YAML
compiler.  The reason this package is called `dhall-json` is that the Haskell
`yaml` library uses the same data structure as Haskell's `aeson` library for
JSON

## Example

```bash
$ dhall-to-json <<< "{ foo = 1, bar = True }"
{"foo":1,"bar":true}
$ dhall-to-json <<< "List/head Natural ([] : List Natural)"
null
$ dhall-to-yaml <<< "{ foo = [1, 2, 3], bar = { baz = True } }"
foo:
- 1
- 2
- 3
bar:
  baz: true
```
