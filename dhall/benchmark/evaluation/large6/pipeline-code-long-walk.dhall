-- Compare plain `Code` import of a package with one frozen walk-heavy child.
let pkg = ./package-long-walk.dhall

in  pkg.result
