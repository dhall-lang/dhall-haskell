
let iterate
    =   λ(n : Natural)
      → λ(a : Type)
      → λ(f : a → a)
      → λ(x : a)
      → List/build
        a
        (   λ(list : Type)
          → λ(cons : a → list → list)
          → List/fold
            { index : Natural, value : {} }
            ( List/indexed
              {}
              ( List/build
                {}
                (   λ(list : Type)
                  → λ(cons : {} → list → list)
                  → Natural/fold n list (cons {=})
                )
              )
            )
            list
            (   λ(y : { index : Natural, value : {} })
              → cons (Natural/fold y.index a f x)
            )
        )

let countTo =
  λ (x : Natural)
  → iterate x Natural (λ (x : Natural) → x + 1) 0

let sum =
  λ (xs : List Natural)
  → List/fold Natural xs Natural (λ (x : Natural) → λ (acc : Natural) → x + acc) 0


let map
        : ∀(a : Type) → ∀(b : Type) → (a → b) → List a → List b
        =   λ(a : Type)
          → λ(b : Type)
          → λ(f : a → b)
          → λ(xs : List a)
          → List/build
            b
            (   λ(list : Type)
              → λ(cons : b → list → list)
              → List/fold a xs list (λ(x : a) → cons (f x))
            )

let any
        : ∀(a : Type) → (a → Bool) → List a → Bool
        =   λ(a : Type)
          → λ(f : a → Bool)
          → λ(xs : List a)
          → List/fold a xs Bool (λ(x : a) → λ(r : Bool) → f x || r) False

let filter
        : ∀(a : Type) → (a → Bool) → List a → List a
        =   λ(a : Type)
          → λ(f : a → Bool)
          → λ(xs : List a)
          → List/build
            a
            (   λ(list : Type)
              → λ(cons : a → list → list)
              → List/fold
                a
                xs
                list
                (λ(x : a) → λ(xs : list) → if f x then cons x xs else xs)
            )

in sum (filter Natural Natural/even
     (map Natural Natural (λ(x:Natural) → x + 10) (countTo 1000)))
