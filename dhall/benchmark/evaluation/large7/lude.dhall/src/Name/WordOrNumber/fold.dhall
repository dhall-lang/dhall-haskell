let Self = ./Type.dhall

let LatinChars = ../../LatinChars/package.dhall

in  \(Out : Type) ->
    \(fromWord : LatinChars.Type -> Out) ->
    \(fromNumber : Natural -> Out) ->
    \(self : Self) ->
      merge { Word = fromWord, Number = fromNumber } self
