let Prelude = ../../Deps/Prelude.dhall

let LatinChars = ../../LatinChars/package.dhall

in  ./fold.dhall Text LatinChars.toTextInLower Prelude.Natural.show
