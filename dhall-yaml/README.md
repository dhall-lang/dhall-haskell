# `dhall-yaml`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

Full documentation here:

* [`dhall-yaml` instructions](https://hackage.haskell.org/package/dhall-yaml/docs/Dhall-Yaml.html)

## Introduction

This `dhall-yaml` package provides a Dhall to YAML compiler, `dhall-to-yaml-ng`,
and a tool for deriving Dhall from YAML code: `yaml-to-dhall`.

Note that the `dhall-json` package also provides a `dhall-to-yaml` executable.
`dhall-to-yaml-ng` renders YAML with the `yamlet` package. The conversion
rules are in [`yaml-conversion.md`](yaml-conversion.md). `dhall-to-yaml` uses
`aeson-yaml` and does not follow every rule there, in particular the quoting
of YAML 1.1 bool words.

This package builds with GHC 9.4 and later. The GHC 8.10 and GHC 9.2 Stack
resolvers do not build it: `yamlet` needs `text >= 2.0.2`, and the GHC 9.2
snapshot pins `text` 1.2.

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
