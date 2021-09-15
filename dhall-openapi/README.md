# `dhall-openapi`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

## Introduction

This `dhall-openapi` package provides a compiler from OpenAPI 2.0 (a.k.a. Swagger)
to Dhall.

The `openapi-to-dhall` executable generates the Dhall types from an OpenAPI
specification.

The simplest way to use the executable is:

```bash
$ openapi-to-dhall openapi.json
```

… where `openapi.json` could be any OpenAPI 2.0 document.

For example,
[`dhall-kubernetes`](https://github.com/dhall-lang/dhall-kubernetes) generates
Dhall bindings for each version of the Kubernetes API by running
`openapi-to-dhall` on a `swagger.json` file like
[this one](https://raw.githubusercontent.com/kubernetes/kubernetes/master/api/openapi-spec/swagger.json).

The `openapi-to-dhall` command generates the following directories and files:

* `./types` - This directory contains one Dhall type for each definition in the
  OpenAPI Definitions object

  In other words, this generates one Dhall type for each sub-key of the
  `"definitions"` key

* `./defaults` - This directory contains the corresponding default values for
  each Dhall type in the `./types` directory

* `./schemas` - This directory contains the corresponding "schemas" for each
  definition

  A schema is a value suitable for Dhall's record completion operator (i.e.
  `::`) containing both the type and the default value bundled together.  Each
  of the schemas contained in this directory just re-exports the corresponding
  type and default value from the other two directories.

* `./types.dhall` - A record which re-exports all of the types in the `./types`
  directory

* `./defaults.dhall` - A record which re-exports all of the defaults in the
  `./defaults` directory

* `./schemas.dhall` - A record which re-exports all of the schemas in the
  `./schemas` directory

* `./typesUnion.dhall` - A record which exports a union type with one
  alternative for each type

* `./package.dhall` - A record which re-exports `schemas.dhall` and
  `typesUnion.dhall`

For example, see the
[Dhall code generated for Kubernetes version 1.21](https://github.com/dhall-lang/dhall-kubernetes/tree/master/1.21).
