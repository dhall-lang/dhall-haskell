{ example0 =
      λ ( f
        :   ∀(optional : Type)
          → ∀(just : Text → optional)
          → ∀(nothing : optional)
          → optional
        )
    → f
, example1 = [ "foo" ] : Optional Text
}
