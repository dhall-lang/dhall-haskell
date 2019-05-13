let Sigma =
        λ(A : Type)
      → λ(P : A → Type)
      → < intro : ∀(r : Type) → (∀(a : A) → P a → r) → r >

let Pi = λ(A : Type) → λ(P : A → Type) → < intro : ∀(a : A) → P a >

let constT = λ(X : Type) → λ(Y : Type) → λ(_ : Y) → X : Type

let SigmaPair = λ(A : Type) → λ(B : Type) → Sigma A (constT B A) : Type

let SigmaMkPair =
        λ(A : Type)
      → λ(B : Type)
      → λ(a : A)
      → λ(b : B)
      → (SigmaPair A B).intro (λ(r : Type) → λ(f : ∀(a : A) → B → r) → f a b)

let SigmaFst =
        λ(A : Type)
      → λ(B : Type)
      → λ(sp : SigmaPair A B)
      → merge
        { intro =
              λ(f : ∀(r : Type) → (∀(a : A) → B → r) → r)
            → f A (λ(a : A) → λ(_ : B) → a)
        }
        sp
        : A

let SigmaSnd =
        λ(A : Type)
      → λ(B : Type)
      → λ(sp : SigmaPair A B)
      → merge
        { intro =
              λ(f : ∀(r : Type) → (∀(a : A) → B → r) → r)
            → f B (λ(_ : A) → λ(b : B) → b)
        }
        sp
        : B

let PiFunction = λ(A : Type) → λ(B : Type) → Pi A (constT B A) : Type

let PiMkFunction =
      λ(A : Type) → λ(B : Type) → λ(f : A → B) → (PiFunction A B).intro f

let PiApply =
        λ(A : Type)
      → λ(B : Type)
      → λ(pf : PiFunction A B)
      → λ(a : A)
      → merge { intro = λ(f : ∀(a : A) → B) → f a } pf : B

let bool = λ(A : Type) → λ(B : Type) → λ(t : Bool) → if t then A else B : Type

let SigmaSum = λ(A : Type) → λ(B : Type) → Sigma Bool (bool A B)

let SigmaSumLeft =
        λ(A : Type)
      → λ(B : Type)
      → λ(a : A)
      → (SigmaSum A B).intro
        (λ(r : Type) → λ(f : ∀(a : Bool) → bool A B a → r) → f True a)

let SigmaSumRight =
        λ(A : Type)
      → λ(B : Type)
      → λ(b : B)
      → (SigmaSum A B).intro
        (λ(r : Type) → λ(f : ∀(a : Bool) → bool A B a → r) → f False b)

let prodPredicate =
      λ(A : Type) → λ(B : Type) → λ(R : Type) → ((A → R) → (B → R) → R) : Type

let PiKT = λ(A : Kind) → λ(P : A → Type) → < intro : ∀(a : A) → P a >

let PiSum = λ(A : Type) → λ(B : Type) → PiKT Type (prodPredicate A B) : Type

let PiSumLeft =
        λ(A : Type)
      → λ(B : Type)
      → λ(a : A)
      → (PiSum A B).intro (λ(R : Type) → λ(f : A → R) → λ(g : B → R) → f a)

let PiSumRight =
        λ(A : Type)
      → λ(B : Type)
      → λ(b : B)
      → (PiSum A B).intro (λ(R : Type) → λ(f : A → R) → λ(g : B → R) → g b)

let PiSumElim =
        λ(A : Type)
      → λ(B : Type)
      → λ(R : Type)
      → λ(f : A → R)
      → λ(g : B → R)
      → λ(ps : PiSum A B)
      → merge
        { intro = λ(fn : ∀(R : Type) → (A → R) → (B → R) → R) → fn R f g }
        ps
        : R

let PiPair = λ(A : Type) → λ(B : Type) → Pi Bool (bool A B) : Type

let PiFst =
        λ(A : Type)
      → λ(B : Type)
      → λ(pp : PiPair A B)
      → merge { intro = λ(fn : ∀(x : Bool) → bool A B x) → fn True } pp : A

let PiSnd =
        λ(A : Type)
      → λ(B : Type)
      → λ(pp : PiPair A B)
      → merge { intro = λ(fn : ∀(x : Bool) → bool A B x) → fn False } pp : B

let sigmaTuple = SigmaMkPair Bool Natural True 2

in  { pi =
        { apply =
            PiApply
            Natural
            Natural
            (PiMkFunction Natural Natural (λ(x : Natural) → x + 1))
            1
        , sumElim =
            PiSumElim
            Natural
            Bool
            Text
            (λ(_ : Natural) → "left")
            (λ(_ : Bool) → "right")
            (PiSumLeft Natural Bool 1)
        }
    , sigma =
        { fst =
            SigmaFst Bool Natural sigmaTuple
        , snd =
            SigmaSnd Bool Natural sigmaTuple
        }
    }
