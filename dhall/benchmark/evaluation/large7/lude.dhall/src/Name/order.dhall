let Self = ./Type.dhall

let Typeclasses = ../Deps/Typeclasses.dhall

let Ordering = Typeclasses.Classes.Ordering

let Word = ../LatinChars/package.dhall

let WordOrNumber = ./WordOrNumber/package.dhall

let OrderType = < Less | Equal | Greater >

let compare =
      \(left : Self) ->
      \(right : Self) ->
        let comparison = Word.order.order left.head right.head

        let handler =
              { Less = OrderType.Less
              , Greater = OrderType.Greater
              , Equal =
                  ( Typeclasses.Instances.List.ordering
                      WordOrNumber.Type
                      WordOrNumber.order
                  ).order
                    left.tail
                    right.tail
              }

        in  merge handler comparison

in  { order = compare } : Ordering.Type Self
