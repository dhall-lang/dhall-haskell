{ example0 =
    λ(a : Type) →
    λ(o : Optional a) →
    λ(optional : Type) →
    λ(some : a → optional) →
    λ(none : optional) →
      merge { Some = some, None = none } o
, example1 =
    λ(a : Type) →
    λ ( build
      : ∀(optional : Type) →
        ∀(some : a → optional) →
        ∀(none : optional) →
          optional
      ) →
      build (Optional a) (λ(x : a) → Some x) (None a)
, example2 = merge { Some = Natural/even, None = False } (Some 1)
}
