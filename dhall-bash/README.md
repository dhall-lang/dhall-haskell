# `dhall-bash 1.0.16`

[![Hackage](https://img.shields.io/hackage/v/dhall-bash.svg)](https://hackage.haskell.org/package/dhall-bash)

This `dhall-bash` package provides a Dhall to Bash compiler so that you can
easily marshall Dhall values into your Bash scripts

This does not compile all available Dhall language constructs into Bash and
only supports extracting primitive values, lists, optional values and records
from normalized expressions.

## Quick start

If you have Nix installed, then you can build and install this package using:

```bash
$ nix-env --install --file default.nix
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
let replicate = https://ipfs.io/ipfs/QmcTbCdS21pCxXysTzEiucDuwwLWbLUWNSKwkJVfwpy2zK/Prelude/List/replicate
in  replicate +10 Integer 1
<Ctrl-D>
declare -r -a BAR=(1 1 1 1 1 1 1 1 1 1)
$ dhall-to-bash --declare BAZ <<< '{ qux = 1, xyzzy = True }'
declare -r -A BAZ=([qux]=1 [xyzzy]=true)
```
