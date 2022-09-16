{-# LANGUAGE DeriveGeneric #-}

{-| This module contains the core syntax types.
-}

module Dhall.Syntax.FunctionBinding (
      FunctionBinding(..)
    , makeFunctionBinding

    -- * Optics
    , functionBindingExprs
    ) where

import                Data.Text         (Text)
import {-# SOURCE #-} Dhall.Syntax.Expr (Expr)
import                GHC.Generics      (Generic)

-- | Record the label of a function or a function-type expression
--
-- For example,
--
-- > λ({- A -} a {- B -} : {- C -} T) -> e
--
-- … will be instantiated as follows:
--
-- * @functionBindingSrc0@ corresponds to the @A@ comment
-- * @functionBindingVariable@ is @a@
-- * @functionBindingSrc1@ corresponds to the @B@ comment
-- * @functionBindingSrc2@ corresponds to the @C@ comment
-- * @functionBindingAnnotation@ is @T@
data FunctionBinding s a = FunctionBinding
    { functionBindingSrc0 :: Maybe s
    , functionBindingVariable :: Text
    , functionBindingSrc1 :: Maybe s
    , functionBindingSrc2 :: Maybe s
    , functionBindingAnnotation :: Expr s a
    } deriving Generic

-- | Smart constructor for 'FunctionBinding' with no src information
makeFunctionBinding :: Text -> Expr s a -> FunctionBinding s a
makeFunctionBinding l t = FunctionBinding Nothing l Nothing Nothing t

{-| Traverse over the immediate 'Expr' children in a 'FunctionBinding'.
-}
functionBindingExprs
    :: Applicative f
    => (Expr s a -> f (Expr s b))
    -> FunctionBinding s a -> f (FunctionBinding s b)
functionBindingExprs f (FunctionBinding s0 label s1 s2 type_) =
    FunctionBinding
        <$> pure s0
        <*> pure label
        <*> pure s1
        <*> pure s2
        <*> f type_
{-# INLINABLE functionBindingExprs #-}
