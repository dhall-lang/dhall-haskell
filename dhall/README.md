# `dhall 1.19.1`

[![Hackage](https://img.shields.io/hackage/v/dhall.svg)](https://hackage.haskell.org/package/dhall)

Dhall is a programmable configuration language that is not Turing-complete

You can think of Dhall as: JSON + functions + types + imports

You will probably want to read the language-agnostic README here:

* [`dhall-lang` `README`](https://github.com/dhall-lang/dhall-lang/blob/master/README.md)

This repository (and this `README`) focuses on the Haskell implementation of
Dhall

## Motivation

*"Why not configure my program using JSON or YAML?"*

JSON or YAML are suitable for small configuration files, but larger
configuration files with complex schemas require programming language features
to reduce repetition.  Otherwise, the repetitive configuration files become
error-prone and difficult to maintain/migrate.

This post explains in more detail the motivation behind programmable
configuration files:

* [Programmable configuration files](https://github.com/dhall-lang/dhall-lang/wiki/Programmable-configuration-files)

*"Why not configure my program using Haskell code?"*

You probably don't want to rebuild your program every time you make a
configuration change.  Recompilation is slow and requires the GHC toolchain
to be installed anywhere you want to make configuration changes.

## Quick start

Given this Haskell program saved to `example.hs`:

```haskell
-- example.hs

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import Dhall

data Example = Example { foo :: Integer, bar :: Vector Double }
    deriving (Generic, Show)

instance Interpret Example

main :: IO ()
main = do
    x <- input auto "./config"
    print (x :: Example)
```

... which reads in this configuration file:

```bash
$ cat ./config
{ foo = 1
, bar = ./bar
}
```

... which in turn references this other file:

```
$ cat ./bar
[3.0, 4.0, 5.0]
```

... you can interpret the Haskell program like this:

```bash
$ nix-shell ../nix/test-dhall.nix
[nix-shell]$ runghc example.hs
Example {foo = 1, bar = [3.0,4.0,5.0]}
```

You can also interpret Dhall programs directly using the installed command-line
compiler:

```bash
$ dhall
List/head Double ./bar
<Ctrl-D>
Optional Double

Some 3.0
```

... and you can reference remote expressions or functions by their URL, too:

```bash
$ dhall
let null = https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/List/null
in  null Double ./bar
<Ctrl-D>
Bool

False
```

Now go read the
[Dhall tutorial](https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html)
to learn more

## Building this project

Nix + Cabal is the recommended workflow for project development since continuous
integration uses Nix to build and test the project.  Other development tools and
workflows are also supported on a best-effort basis.

You can build the project using only Nix by running this command from the root
of the repository:

```bash
$ nix-build
```

More commonly, you will want to incrementally build the project using `cabal`.
You can either do so inside of a `nix-shell`:

```bash
$ nix-shell
[nix-shell]$ cabal configure
[nix-shell]$ cabal build
[nix-shell]$ cabal test
```

... or you can add `nix: True` to your `~/.cabal/config` file and then you can
run the same `cabal` commands without an explicit `nix-shell`:

```bash
$ cabal configure
$ cabal build
$ cabal test
```
