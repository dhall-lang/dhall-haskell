let ExprF = ./exprf.dhall

let Expr
  = λ(e : Type)
  → λ(f : ExprF e → e)
  → let E = ExprF e in
    { Lit = λ(x : Natural)      → f (E.LitF { _1 = x })
    , Add = λ(a : e) → λ(b : e) → f (E.AddF { _1 = a, _2 = b })
    , Mul = λ(a : e) → λ(b : e) → f (E.MulF { _1 = a, _2 = b })
    }

in Expr
