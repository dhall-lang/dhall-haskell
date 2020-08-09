{-|
A complex example of jump-to-definition on let-bindings
-}
let fun =
    λ(a : Text) ->
    λ(b : Text) ->
    λ(c : Text) ->
    λ(d : Text) ->
    [ a ++ b, b ++ c, c ++ d, d ++ a ]

in fun
