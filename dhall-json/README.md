# `dhall-json`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

Full documentation here:

* [`dhall-json` instructions](https://hackage.haskell.org/package/dhall-json/docs/Dhall-JSON.html)

## Introduction

This `dhall-json` package provides a Dhall to JSON compiler, and a Dhall to YAML
compiler based on that. The `dhall-to-yaml` executable is a "basic" version of
the `dhall-to-yaml-ng` executable in the `dhall-yaml` package.

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

## Development

This package's `dhall-to-yaml` and `dhall-yaml`'s `dhall-to-yaml-ng` should be
kept as closely in sync as possible. Common code for these executables lives
in this package, tests live in `dhall-yaml`.
