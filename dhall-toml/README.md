# `dhall-toml`

**NOTE:** this package has not been completed see the
[roadmap section](#roadmap)

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

Full documentation is available on Hackage:

* [`dhall-toml` on Hackage](https://hackage.haskell.org/package/dhall-toml)

## Introduction

This `dhall-toml` package provides a Dhall to TOML compiler.

## Example

```bash
$ dhall-to-toml <<< "{ foo = 1, bar = True }"
foo = 1
bar = true
```

## Roadmap
* [x] - minimal `dhall-to-toml`
* [ ] - schema inference for `toml-to-dhall`
* [x] - minimal `toml-to-dhall`
* [ ] - documentation in docs.dhall-lang.org

