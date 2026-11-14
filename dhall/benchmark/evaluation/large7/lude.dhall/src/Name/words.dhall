let Prelude = ../Deps/Prelude.dhall

let Self = ./Type.dhall

let LatinChar = ../LatinChar/package.dhall

let LatinChars = ../LatinChars/package.dhall

let uncons = ../List/uncons.dhall

let word = ./word.dhall

let concat = ./concat.dhall

let fromLatinChars = ./fromLatinChars.dhall

in  \(headHead : LatinChar.Type) ->
    \(headTail : List LatinChar.Type) ->
    \(tail : List (List LatinChar.Type)) ->
        concat
          (word headHead headTail)
          ( Prelude.List.filterMap
              (List LatinChar.Type)
              Self
              ( Prelude.Function.compose
                  (List LatinChar.Type)
                  (Optional LatinChars.Type)
                  (Optional Self)
                  (uncons LatinChar.Type)
                  (Prelude.Optional.map LatinChars.Type Self fromLatinChars)
              )
              tail
          )
      : Self
