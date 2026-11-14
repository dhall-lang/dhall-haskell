--| `lessThan` checks if one Natural is strictly less than another.
let greaterThanEqual = ./greaterThanEqual.dhall

let Bool/not = ../Bool/not.dhall

let lessThan
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → Bool/not (greaterThanEqual x y)

let example0 = assert : lessThan 5 6 ≡ True

let example1 = assert : lessThan 5 5 ≡ False

let example2 = assert : lessThan 5 4 ≡ False

let property0 = λ(n : Natural) → assert : lessThan n 0 ≡ False

let property1 = λ(n : Natural) → assert : lessThan n n ≡ False

in  lessThan
