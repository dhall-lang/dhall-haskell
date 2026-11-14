-- Convert a sequence of Latin characters to a title-cased text.
-- The first character is converted to uppercase, and the rest to lowercase.
let Self = ./Type.dhall

let Prelude = ../Deps/Prelude.dhall

let Char = ../LatinChar/package.dhall

in  \(self : Self) ->
          Char.toTextInUpper self.head
      ++  Prelude.Text.concatMap Char.Type Char.toTextInLower self.tail
