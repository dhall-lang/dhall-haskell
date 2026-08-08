-- Same as pipeline-code-long-typecheck.dhall, but the package is imported `as Source`.
let pkg = ./package-long-typecheck.dhall as Source

in  pkg.result
