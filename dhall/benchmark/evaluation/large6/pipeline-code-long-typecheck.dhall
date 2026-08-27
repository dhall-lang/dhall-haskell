-- Compare plain `Code` import of a package with one frozen typecheck-heavy child.
let pkg = ./package-long-typecheck.dhall

in  pkg.result
