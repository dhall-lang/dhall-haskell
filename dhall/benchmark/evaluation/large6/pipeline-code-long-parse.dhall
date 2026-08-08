-- Compare plain `Code` import of a package with one frozen parse-heavy child.
let pkg = ./package-long-parse.dhall

in  pkg.result
