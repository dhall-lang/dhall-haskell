{ example0 =
    Natural/fold
    ( Natural/build
      (   λ(natural : Type)
        → λ(succ : natural → natural)
        → λ(zero : natural)
        → succ zero
      )
    )
, example1 =
    Natural/fold
    ( Natural/build
      (   λ(natural : Type)
        → λ(succ : natural → natural)
        → λ(zero : natural)
        → succ zero
      )
    )
    Text
    (λ(t : Text) → t ++ "!")
, example2 =
    Natural/fold
    ( Natural/build
      (   λ(natural : Type)
        → λ(succ : natural → natural)
        → λ(zero : natural)
        → succ zero
      )
    )
    Text
    (λ(t : Text) → t ++ "!")
    "You're welcome"
, example3 =
    Natural/fold
    (     let one =
                Natural/build
                (   λ(natural : Type)
                  → λ(succ : natural → natural)
                  → λ(zero : natural)
                  → succ zero
                )
      
      in  one
    )
}
