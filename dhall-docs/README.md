# `dhall-docs`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

## Introduction

This `dhall-docs` package provides a cli utility that takes a dhall package or file and outputs
a HTML documentation of it.

## Usage

The easiest usage is the following:

```bash
dhall-docs --input <YOUR-PACKAGE-FOLDER>
```

By default it will save the documentation of your package in `$PWD/docs`, but
you can change that using the `--output` flag

```bash
dhall-docs --input . --output other-dir
```

