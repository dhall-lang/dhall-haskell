module Dhall.Core where

data Const

data Var

data Expr s a

data Import

denote :: Expr s a -> Expr t a
