-- Same as pipeline-code-long-normalize.dhall, but the package is imported `as Source`.
let pkg = ./package-long-normalize.dhall as Source

in  pkg.result
