{ example0 = [ +1 ] : Optional Natural
, example1 = [ 1 ] : Optional Integer
, example2 =
      λ(id : ∀(a : Type) → a → a)
    → Optional/build
      Bool
      (   λ(optional : Type)
        → λ(just : Bool → optional)
        → λ(nothing : optional)
        → id optional (just True)
      )
}
