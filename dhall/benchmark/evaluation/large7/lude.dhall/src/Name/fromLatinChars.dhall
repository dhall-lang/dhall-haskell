let Name = ./Type.dhall

let WordOrNumber = ./WordOrNumber/package.dhall

let LatinChars = ../LatinChars/package.dhall

in  \(latinChars : LatinChars.Type) ->
      { head = latinChars, tail = [] : List WordOrNumber.Type } : Name
