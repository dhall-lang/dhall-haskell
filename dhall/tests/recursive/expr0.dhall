  λ(Expr : Type)
→ let ExprF =
        < LitF : Natural
        | AddF :
            { _1 : Expr, _2 : Expr }
        | MulF :
            { _1 : Expr, _2 : Expr }
        >
  
  in    λ(Fix : ExprF → Expr)
      → let Lit = λ(x : Natural) → Fix (ExprF.LitF x)
        
        let Add =
              λ(x : Expr) → λ(y : Expr) → Fix (ExprF.AddF { _1 = x, _2 = y })
        
        let Mul =
              λ(x : Expr) → λ(y : Expr) → Fix (ExprF.MulF { _1 = x, _2 = y })
        
        in  Add (Mul (Lit 3) (Lit 7)) (Add (Lit 1) (Lit 2))
