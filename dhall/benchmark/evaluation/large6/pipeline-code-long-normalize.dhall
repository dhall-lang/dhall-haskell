-- Compare plain `Code` import of a package with one frozen normalize-heavy child.
let pkg = ./package-long-normalize.dhall

in  pkg.result
