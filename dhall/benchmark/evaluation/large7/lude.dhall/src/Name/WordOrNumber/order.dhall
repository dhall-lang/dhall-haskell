let Self = ./Type.dhall

let Typeclasses = ../../Deps/Typeclasses.dhall

let Ordering = Typeclasses.Classes.Ordering

let LatinChars = ../../LatinChars/package.dhall

let OrderType = < Less | Equal | Greater >

let compareNatural =
      \(left : Natural) ->
      \(right : Natural) ->
        if    Natural/isZero (Natural/subtract right left)
        then  if    Natural/isZero (Natural/subtract left right)
              then  OrderType.Equal
              else  OrderType.Greater
        else  OrderType.Less

let compare =
      \(left : Self) ->
      \(right : Self) ->
        merge
          { Number =
              \(left : Natural) ->
                merge
                  { Number = \(right : Natural) -> compareNatural left right
                  , Word = \(right : LatinChars.Type) -> OrderType.Less
                  }
                  right
          , Word =
              \(left : LatinChars.Type) ->
                merge
                  { Number = \(right : Natural) -> OrderType.Greater
                  , Word =
                      \(right : LatinChars.Type) ->
                        LatinChars.order.order left right
                  }
                  right
          }
          left

in  { order = compare } : Ordering.Type Self
