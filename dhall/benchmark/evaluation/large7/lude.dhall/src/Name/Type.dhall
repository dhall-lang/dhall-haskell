let Head = ../LatinChars/Type.dhall

let TailElement = ./WordOrNumber/Type.dhall

let Tail = List TailElement

in  { head : Head, tail : Tail }
