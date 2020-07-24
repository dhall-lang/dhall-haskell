
{-|
Just a *Pair* right here
-}

let Pair
  : forall(A: Type) -- | Type of first position
  -> forall(B: Type) -- | Type of second position
  -> Type
  = λ(A: Type) -> λ(B: Type) -> {first: A, second: B}

in Pair
