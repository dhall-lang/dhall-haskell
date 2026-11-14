let Name = ./Type.dhall

let WordOrNumber = ./WordOrNumber/package.dhall

let LatinChars = ../LatinChars/package.dhall

in  \(latinChars : LatinChars.Type) ->
    \(number : Natural) ->
      { head = latinChars, tail = [ WordOrNumber.Type.Number number ] } : Name
