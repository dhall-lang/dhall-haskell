{-|
`greaterThanEqual` checks if one Natural is greater than or equal to another.
-}
let lessThanEqual = ./lessThanEqual.dhall

let greaterThanEqual
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → lessThanEqual y x

let example0 = assert : greaterThanEqual 5 6 ≡ False

let example1 = assert : greaterThanEqual 5 5 ≡ True

let example2 = assert : greaterThanEqual 5 4 ≡ True

let property0 = λ(n : Natural) → assert : greaterThanEqual n 0 ≡ True

let property1 = λ(n : Natural) → assert : greaterThanEqual n n ≡ True

in  greaterThanEqual
