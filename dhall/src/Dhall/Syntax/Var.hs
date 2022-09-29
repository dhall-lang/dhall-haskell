{-# LANGUAGE DeriveGeneric #-}

module Dhall.Syntax.Var
    ( Var(..)
    ) where

import Data.String  (IsString (..))
import Data.Text    (Text)
import GHC.Generics (Generic)

{-| Label for a bound variable

    The `Data.Text.Text` field is the variable's name (i.e. \"@x@\").

    The `Int` field disambiguates variables with the same name if there are
    multiple bound variables of the same name in scope.  Zero refers to the
    nearest bound variable and the index increases by one for each bound
    variable of the same name going outward.  The following diagram may help:

>                               ┌──refers to──┐
>                               │             │
>                               v             │
> λ(x : Type) → λ(y : Type) → λ(x : Type) → x@0
>
> ┌─────────────────refers to─────────────────┐
> │                                           │
> v                                           │
> λ(x : Type) → λ(y : Type) → λ(x : Type) → x@1

    This `Int` behaves like a De Bruijn index in the special case where all
    variables have the same name.

    You can optionally omit the index if it is @0@:

>                               ┌─refers to─┐
>                               │           │
>                               v           │
> λ(x : Type) → λ(y : Type) → λ(x : Type) → x

    Zero indices are omitted when pretty-printing @Var@s and non-zero indices
    appear as a numeric suffix.
-}
data Var = V Text !Int
    deriving Generic

instance IsString Var where
    fromString str = V (fromString str) 0
