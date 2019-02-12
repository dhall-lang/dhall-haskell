# `dhall`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

Full documentation here:

* [`dhall` instructions](https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html)

## Introduction

Dhall is a programmable configuration language that is not Turing-complete

You can think of Dhall as: JSON + functions + types + imports

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

## Example

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

Now go read the [Dhall tutorial][haskell-tutorial] to learn more.

## Standard-compatibility table

| Haskell package version | Supported standard version |
|-------------------------|----------------------------|
| `1.20.*`                | `5.0.0`                    |
| `1.19.*`                | `4.0.0`                    |
| `1.18.*`                | `3.0.0`                    |
| `1.17.*`                | `2.0.0`                    |
| `1.16.*`                | `1.0.0`                    |

[haskell-tutorial]: https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html
