let Self = ./Type.dhall

let Prelude = ../Deps/Prelude.dhall

let Word = ../LatinChars/package.dhall

let WordOrNumber = ./WordOrNumber/package.dhall

in  \(self : Self) ->
          Word.toTextInLower self.head
      ++  Prelude.Text.concatMap
            WordOrNumber.Type
            WordOrNumber.toTextInUpper
            self.tail
