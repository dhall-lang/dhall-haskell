  λ(a : Type)
→ let ExprF =
        < LitF :
            { _1 : Natural }
        | AddF :
            { _1 : a, _2 : a }
        | MulF :
            { _1 : a, _2 : a }
        >
  
  in    λ(a : ExprF → a)
      → let Lit = λ(x : Natural) → a (ExprF.LitF { _1 = x })
        
        let Add = λ(x : a@1) → λ(y : a@1) → a (ExprF.AddF { _1 = x, _2 = y })
        
        let Mul = λ(x : a@1) → λ(y : a@1) → a (ExprF.MulF { _1 = x, _2 = y })
        
        in  Add (Mul (Lit 3) (Lit 7)) (Add (Lit 1) (Lit 2))
