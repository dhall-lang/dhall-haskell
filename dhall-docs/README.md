# `dhall-docs`

:construction: **This tool is on development phase yet. It is not usable right now.**

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

## Introduction

This `dhall-docs` package provides a cli utility that takes a dhall package or file and outputs
a HTML documentation of it.

## Usage

The easiest usage is the following:

```bash
dhall-docs --input ${PACKAGE-FOLDER}
```

By default it will save the documentation of your package in `./docs`, but
you can change that using the `--output` flag

```bash
dhall-docs --input . --output ${OTHER_DIR}
```

## Development

If you want to open the `ghci` repl on this package using `stack`, you have to
provide an additional flag:

```bash
stack ghci dhall-docs --flag dhall-docs:ghci-data-files
```

... otherwise the file-embedded css and fonts won't be properly linked
