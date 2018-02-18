{ example0 =
      λ ( f
        :   ∀(optional : Type)
          → ∀(just : Text → optional)
          → ∀(nothing : optional)
          → optional
        )
    → Optional/fold Text (Optional/build Text f)
, example1 =
    Optional/build Text (Optional/fold Text ([ "foo" ] : Optional Text))
}
