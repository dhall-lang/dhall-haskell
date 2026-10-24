let Self = ./Type.dhall

let Prelude = ../Deps/Prelude.dhall

let Char = ../LatinChar/package.dhall

in  \(self : Self) ->
          Char.toTextInLower self.head
      ++  Prelude.Text.concatMap Char.Type Char.toTextInLower self.tail
