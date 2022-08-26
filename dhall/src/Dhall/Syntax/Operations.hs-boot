module Dhall.Syntax.Operations where

import Dhall.Syntax.Types (Expr)

unsafeSubExpressions :: Applicative f => (Expr s a -> f (Expr t b)) -> Expr s a -> f (Expr t b)
