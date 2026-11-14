let F
    : Type → Type
    = λ(t : Type) → { l : t, r : t }

let twice
    : ∀(t : Type) → t → F t
    = λ(t : Type) → λ(x : t) → { l = x, r = x }

let double
    : ∀(g : Type → Type) →
      (∀(t : Type) → t → g t) →
      ∀(t : Type) →
        t →
          g (g t)
    = λ(g : Type → Type) →
      λ(step : ∀(t : Type) → t → g t) →
      λ(t : Type) →
      λ(x : t) →
        step (g t) (step t x)

let compose
    : (Type → Type) → (Type → Type) → Type → Type
    = λ(g : Type → Type) →
      λ(h : Type → Type) →
      λ(t : Type) →
        g (h t)

let g1 = F

let g2
    : Type → Type
    = compose g1 g1

let g4
    : Type → Type
    = compose g2 g2

let g8
    : Type → Type
    = compose g4 g4

let s1 = twice

let s2 = double g1 s1

let s4 = double g2 s2

let s8 = double g4 s4

let large = s2 (g8 Natural) (s8 Natural 0)

in  large
