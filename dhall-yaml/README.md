# `dhall-yaml`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

Full documentation here:

* [`dhall-yaml` instructions](https://hackage.haskell.org/package/dhall-yaml/docs/Dhall-Yaml.html)

## Introduction

This `dhall-yaml` package provides a Dhall to YAML compiler, `dhall-to-yaml-ng`,
and a tool for deriving Dhall from YAML code: `yaml-to-dhall`.

Note that the `dhall-json` package also provides a `dhall-to-yaml` executable.
The main benefit of this package is that it depends on `HsYAML` (a pure Haskell
implementation of YAML) instead of the `yaml` package (which depends on the
C `libyaml` package).  This means, for example, that this package can be built
using GHCJS (and in fact this package is built using GHCJS to power
[dhall-lang.org](https://dhall-lang.org)).  Other than that, the
`dhall-to-yaml` and `dhall-to-yaml-ng` executables should behave the same.

Tutorials for the analogous JSON tools are available in the `dhall-json`
package:

* [`dhall-to-json`](https://hackage.haskell.org/package/dhall-json/docs/Dhall-JSON.html)

* [`json-to-dhall`](https://hackage.haskell.org/package/dhall-json/docs/Dhall-JSONToDhall.html)

## Example

```bash
$ dhall-to-yaml-ng <<< "{ foo = [1, 2, 3], bar = { baz = True } }" > example.yaml
$ cat example.yaml
bar:
  baz: true
foo:
- 1
- 2
- 3
$ yaml-to-dhall '{ foo : List Natural, bar : { baz : Bool } }' < example.yaml
{ bar = { baz = True }, foo = [ 1, 2, 3 ] }
```
