{-|
Equality typeclass for types that support equality comparison.

Requires: `equal : A -> A -> Bool`
-}
\(A : Type) -> { equal : A -> A -> Bool }
