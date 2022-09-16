{-# LANGUAGE DeriveGeneric #-}

{-| This module contains the core syntax types.
-}

module Dhall.Syntax.Types (
    -- * 'Expr'
      CharacterSet(..)
    , DhallDouble(..)
    , PreferAnnotation(..)
    , FunctionBinding(..)
    , makeFunctionBinding
    , FieldSelection(..)
    , makeFieldSelection
    , WithComponent(..)
    ) where

import                Data.Text             (Text)
import {-# SOURCE #-} Dhall.Pretty.Internal (CharacterSet (..))
import                Dhall.Syntax.Expr     (Expr (..))
import                GHC.Generics          (Generic)

-- $setup
-- >>> import Dhall.Binary () -- For the orphan instance for `Serialise (Expr Void Import)`

-- | This wrapper around 'Prelude.Double' exists for its 'Eq' instance which is
-- defined via the binary encoding of Dhall @Double@s.
newtype DhallDouble = DhallDouble { getDhallDouble :: Double }
    deriving Generic

-- | Used to record the origin of a @//@ operator (i.e. from source code or a
-- product of desugaring)
data PreferAnnotation
    = PreferFromSource
    | PreferFromCompletion
    deriving Generic

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

-- | Record the field on a selector-expression
--
-- For example,
--
-- > e . {- A -} x {- B -}
--
-- … will be instantiated as follows:
--
-- * @fieldSelectionSrc0@ corresponds to the @A@ comment
-- * @fieldSelectionLabel@ corresponds to @x@
-- * @fieldSelectionSrc1@ corresponds to the @B@ comment
--
-- Given our limitation that not all expressions recover their whitespaces, the
-- purpose of @fieldSelectionSrc1@ is to save the 'Text.Megaparsec.SourcePos'
-- where the @fieldSelectionLabel@ ends, but we /still/ use a 'Maybe Src'
-- (@s = 'Src'@) to be consistent with similar data types such as 'Binding', for
-- example.
data FieldSelection s = FieldSelection
    { fieldSelectionSrc0 :: Maybe s
    , fieldSelectionLabel :: !Text
    , fieldSelectionSrc1 :: Maybe s
    } deriving Generic

-- | Smart constructor for 'FieldSelection' with no src information
makeFieldSelection :: Text -> FieldSelection s
makeFieldSelection t = FieldSelection Nothing t Nothing

-- | A path component for a @with@ expression
data WithComponent = WithLabel Text | WithQuestion
    deriving Generic
