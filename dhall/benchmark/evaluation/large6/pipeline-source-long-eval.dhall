-- Same as pipeline-code-long-eval.dhall, but the package is imported `as Source`.
let pkg = ./package-long-eval.dhall as Source

in  pkg.result
