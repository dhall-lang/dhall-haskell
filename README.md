# `dhall-haskell`

This repository is a shared repository for all of the `dhall-*` Haskell
packages, including:

* [`dhall`](./dhall)
* [`dhall-bash`](./dhall-bash)
* [`dhall-json`](./dhall-json)
* [`dhall-text`](./dhall-text)

Navigate to each package's directory for their respective `README`s

# Quick start

## Building from source

### [cabal](https://www.haskell.org/cabal)

You can build all of the packages by running:

```console
$ cabal new-build all
```

And each of them with `cabal new-build <package-name>`, for example:

```console
$ cabal new-build dhall
```

... or you can run `cabal new-build` within each package directory.

### [nix](https://nixos.org/nix/)

You can build all of the packages by running:

```console
$ nix-build
```

... or you can run `nix-build` within each package's respective directory to
build just that one package.

### [stack](https://docs.haskellstack.org)

You can build all of the packages with

```console
$ stack build
```

And each of them with `stack build <package-name>`, for example:

```console
$ stack build dhall-json
```


## Development status

[![Build Status](https://travis-ci.org/dhall-lang/dhall-haskell.png)](https://travis-ci.org/dhall-lang/dhall-haskell)

The compiler is built upon a theoretically sound foundation, meaning that if
there are no bugs then the language will never crash and will always halt.
However, in practice the compiler needs to be battle-tested to weed out any
implementation bugs, so please open issues! 🙂

Read the following guide if you would like to contribute:

* [Contributing to Dhall](https://github.com/dhall-lang/dhall-lang/blob/master/.github/CONTRIBUTING.md)
