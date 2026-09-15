-- Same as pipeline-code-long-walk.dhall, but the package is imported `as Source`.
let pkg = ./package-long-walk.dhall as Source

in  pkg.result
