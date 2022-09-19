{-# LANGUAGE DeriveGeneric #-}

module Dhall.Syntax.Types
    ( DhallDouble(..)
    , PreferAnnotation(..)
    , FieldSelection(..)
    , makeFieldSelection
    , WithComponent(..)
    ) where

import Data.Text    (Text)
import GHC.Generics (Generic)

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
-- where the @fieldSelectionLabel@ ends, but we /still/ use a
-- 'Maybe Dhall.Src.Src' (@s = 'Dhall.Src.Src'@) to be consistent with similar
-- data types such as 'Dhall.Syntax.Binding.Binding', for example.
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
