-- Same as pipeline-code-long-multi.dhall, but the package is imported `as Source`.
let pkg = ./package-long-multi.dhall as Source

in  pkg.result
