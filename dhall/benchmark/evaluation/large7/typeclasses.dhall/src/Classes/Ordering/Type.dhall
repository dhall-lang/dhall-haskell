{-|
Ordering typeclass for types that support total ordering and comparison.

Requires: `order : A -> A -> Order`
-}
\(A : Type) -> { order : A -> A -> ./Order/Type.dhall }
