-- Compare plain `Code` import of a package with one frozen eval-heavy child.
let pkg = ./package-long-eval.dhall

in  pkg.result
