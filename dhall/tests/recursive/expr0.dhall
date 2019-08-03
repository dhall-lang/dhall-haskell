let ExprF = ./exprf.dhall
let Expr  = ./expr.dhall

-- This code is deliberately convoluted to test shadowing, normalization, etc.
let h = 8

let result
  = λ(a : Type)
  → λ(h : ExprF a → a)
  → let f = h
    let g = a
    let a = 1
    let h = 1
    let E = Expr g f
    in E.Lit (h + a)

in result
