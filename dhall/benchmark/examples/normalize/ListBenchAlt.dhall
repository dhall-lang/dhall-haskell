
let iterate =
    λ(n : Natural)
  → λ(a : Type)
  → λ(f : a → a)
  → λ(x : a)
  → (Natural/fold n
      (a → List a → {fst:a, snd:List a})
	  (λ (hyp : a → List a → {fst:a, snd:List a}) →
	   λ (x : a) → λ (xs : List a)
	   → let tup = hyp x xs
	     in {fst = f (tup.fst), snd = tup.snd # [tup.fst]})
	  (λ (x : a) → λ (xs : List a) → {fst=x, snd=xs})
	  x ([] : List a)).snd

let countTo =
  λ (x : Natural)
  → iterate x Natural (λ (x : Natural) → x + 1) 0

let sum =
  λ (xs : List Natural)
  → List/fold Natural xs Natural (λ (x : Natural) → λ (acc : Natural) → x + acc) 0

let map
  =   λ(a : Type)
    → λ(b : Type)
    → λ(f : a → b)
    → λ(xs : List a)
	→ List/fold a xs (List b) (λ (x : a) → λ (xs : List b) → [f x] # xs) ([] : List b)

let any
  =   λ(a : Type)
        → λ(f : a → Bool)
        → λ(xs : List a)
        → List/fold a xs Bool (λ(x : a) → λ(r : Bool) → f x || r) False

let filter
  =   λ(a : Type)
    → λ(f : a → Bool)
    → λ(xs : List a)
	→ List/fold a xs (List a)
	    (λ (x : a) → λ (xs : List a) → if f x then [x] # xs else xs) ([] : List a)

in sum (filter Natural Natural/even
     (map Natural Natural (λ(x:Natural) → x + 10) (countTo 1000)))
