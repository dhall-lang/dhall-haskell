{-|
On this example, the first declaration of variable `a` is shadowed with the
later declaration. The jump-to-definintion on the usage of `a`
should not highlight the first declaration, and interacting with the first
declaration should not change the others
-}
let a = 1

let a = 2

in a
