{-|
On this example, we introduce the `a` variable in the two parameters of the
`fun` function and the jump-to-definition engine correctly highlights the usage
when indexes are used
-}
let fun =
    λ(a : Text) ->
    λ(a : Text) ->
    [ a@1 ++ a@0 ]

let example0 = assert : fun "a" "b" === ["ab"]

in fun

