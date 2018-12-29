# `dhall-bash`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

Full documentation here:

* [`dhall-bash` instructions](https://hackage.haskell.org/package/dhall-bash/docs/Dhall-Bash.html)

## Introduction

This `dhall-bash` package provides a Dhall to Bash compiler so that you can
easily marshall Dhall values into your Bash scripts

This does not compile all available Dhall language constructs into Bash and
only supports extracting primitive values, lists, optional values and records
from normalized expressions.

## Example

```bash
$ dhall-to-bash <<< '1'
1
$ dhall-to-bash <<< '"ABC" ++ "DEF"'
ABCDEF
$ dhall-to-bash --declare FOO <<< '"ABC" ++ "DEF"'
declare -r FOO=ABCDEF
$ eval $(dhall-to-bash --declare FOO <<< '"ABC" ++ "DEF"')
$ echo "${FOO}"
ABCDEF
$ dhall-to-bash --declare BAR
let replicate = https://prelude.dhall-lang.org/List/replicate
in  replicate 10 Natural 1
<Ctrl-D>
declare -r -a BAR=(1 1 1 1 1 1 1 1 1 1)
$ dhall-to-bash --declare BAZ <<< '{ qux = 1, xyzzy = True }'
declare -r -A BAZ=([qux]=1 [xyzzy]=true)
```
