{-|
A complex example of the jump-to-definition feature on record literals.
-}
let a =
    { foo = "bar", number = +1 }

let b =
    { foo = "bar", number = +1 }

in a.foo ++ Integer/show a.number ++ b.foo ++ Integer/show b.number
