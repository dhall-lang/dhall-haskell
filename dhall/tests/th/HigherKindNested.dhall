let InnerS = \(x : Type) -> \(y : Type) -> { ifoo : x, ibar : y }

let InnerU = \(x : Type) -> \(y : Type) -> < Ifoo : x | Ibar : y >

let Outer =
      \(i : Type) ->
      \(j : Type) ->
        { ofoo : InnerS i Text, obar : InnerU j Text }

in  { InnerS, InnerU, Outer }
