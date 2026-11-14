let rec =
      { a = 1
      , b = 2
      }
let xs =
      [ 1
      , 2
      ]
let choice =
      if True
      then xs
      else xs
let picked =
      merge
        { A = \(n : Natural) -> n
        , B = 0
        }
        (< A : Natural | B >.A 1)
in  picked
