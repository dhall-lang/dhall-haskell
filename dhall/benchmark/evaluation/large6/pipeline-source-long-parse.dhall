-- Same as pipeline-code-long-parse.dhall, but the package is imported `as Source`.
let pkg = ./package-long-parse.dhall as Source

in  pkg.result
