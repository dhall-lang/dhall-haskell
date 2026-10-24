let LatinChars = ./Type.dhall

let LatinChar = ../LatinChar/package.dhall

in  \(head : LatinChar.Type) ->
    \(tail : List LatinChar.Type) ->
      { head, tail } : LatinChars
