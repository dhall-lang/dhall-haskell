{-|
On this example, the first parameter `a` is shadowed with the second declaration.
The jump-to-definintion on the usage of `a` should not highlight the first declaration,
and interacting with the first declaration should not change the others
-}
let fun =
    λ(a : Text) ->
    λ(a : Text) ->
    [ a ++ a ]

let example0 = assert : fun "a" "b" === ["bb"]

in fun

