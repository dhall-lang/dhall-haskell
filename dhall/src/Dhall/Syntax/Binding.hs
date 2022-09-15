{-# LANGUAGE DeriveGeneric #-}

{-| This module contains the core syntax types.
-}

module Dhall.Syntax.Binding (
      Binding(..)
    , makeBinding

      -- * Optics
    , bindingExprs
    ) where

import                Data.Text             (Text)
import {-# SOURCE #-} Dhall.Syntax.Expr     (Expr)
import                GHC.Generics          (Generic)

-- | Record the binding part of a @let@ expression.
--
-- For example,
--
-- > let {- A -} x {- B -} : {- C -} Bool = {- D -} True in x
--
-- … will be instantiated as follows:
--
-- * @bindingSrc0@ corresponds to the @A@ comment.
-- * @variable@ is @"x"@
-- * @bindingSrc1@ corresponds to the @B@ comment.
-- * @annotation@ is 'Just' a pair, corresponding to the @C@ comment and @Bool@.
-- * @bindingSrc2@ corresponds to the @D@ comment.
-- * @value@ corresponds to @True@.
data Binding s a = Binding
    { bindingSrc0 :: Maybe s
    , variable    :: Text
    , bindingSrc1 :: Maybe s
    , annotation  :: Maybe (Maybe s, Expr s a)
    , bindingSrc2 :: Maybe s
    , value       :: Expr s a
    } deriving Generic

{-| Construct a 'Binding' with no source information and no type annotation.
-}
makeBinding :: Text -> Expr s a -> Binding s a
makeBinding name = Binding Nothing name Nothing Nothing Nothing

{-| Traverse over the immediate 'Expr' children in a 'Binding'.
-}
bindingExprs
  :: (Applicative f)
  => (Expr s a -> f (Expr s b))
  -> Binding s a -> f (Binding s b)
bindingExprs f (Binding s0 n s1 t s2 v) =
  Binding
    <$> pure s0
    <*> pure n
    <*> pure s1
    <*> traverse (traverse f) t
    <*> pure s2
    <*> f v
{-# INLINABLE bindingExprs #-}
