{-|
This tests shows that the feature works for quoted vars on function parameters
as well
-}
let fun =
    λ(`param 1` : Text) ->
    λ(`param 2` : Text) ->
    [ `param 1` ++ `param 2` ]

let example0 = assert : fun "a" "b" === ["ab"]

in fun

