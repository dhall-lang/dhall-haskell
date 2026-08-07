let limit = 300000

let iterate = ../../../dhall-lang/Prelude/List/iterate.dhall

let a =
      List/length
        (List Natural)
        (iterate limit (List Natural) (λ(x : List Natural) → x # [ 1 ]) [ 1 ])

let b = List/length Natural (iterate limit Natural (λ(x : Natural) → x + 1) 1)

in  b
