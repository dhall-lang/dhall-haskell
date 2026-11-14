let Name = ./Type.dhall

let LatinChar = ../LatinChar/package.dhall

in  \(wordHead : LatinChar.Type) ->
    \(wordTail : List LatinChar.Type) ->
      let latinChars = { head = wordHead, tail = wordTail }

      in  ./fromLatinChars.dhall latinChars : Name
