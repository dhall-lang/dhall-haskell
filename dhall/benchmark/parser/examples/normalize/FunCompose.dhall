
let compose
    : ∀(a : Type) → ∀(b : Type) → ∀(c : Type) → (a → b) → (b → c) → a → c
    =   λ(A : Type)
      → λ(B : Type)
      → λ(C : Type)
      → λ(f : A → B)
      → λ(g : B → C)
      → λ(x : A)
      → g (f x)

let composeN : ∀ (a : Type) → Natural → (a → a) → a → a
    = λ (a : Type)
	→ λ (n : Natural)
	→ λ (f : a → a)
	→ Natural/fold n (a → a) (compose a a a f) (λ (x : a) → x)

in composeN Natural 100000 (λ (x : Natural) → x + 1) 0
