{-# LANGUAGE DeriveGeneric #-}

{-| This module contains the core syntax types.
-}

module Dhall.Syntax.Types (
    -- * 'Expr'
      Binding(..)
    , makeBinding
    , CharacterSet(..)
    , Chunks(..)
    , DhallDouble(..)
    , PreferAnnotation(..)
    , RecordField(..)
    , makeRecordField
    , FunctionBinding(..)
    , makeFunctionBinding
    , FieldSelection(..)
    , makeFieldSelection
    , WithComponent(..)
    ) where

import                Data.String           (IsString (..))
import                Data.Text             (Text)
import {-# SOURCE #-} Dhall.Pretty.Internal (CharacterSet (..))
import                Dhall.Syntax.Expr     (Expr (..))
import                GHC.Generics          (Generic)

-- $setup
-- >>> import Dhall.Binary () -- For the orphan instance for `Serialise (Expr Void Import)`

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

-- | This wrapper around 'Prelude.Double' exists for its 'Eq' instance which is
-- defined via the binary encoding of Dhall @Double@s.
newtype DhallDouble = DhallDouble { getDhallDouble :: Double }
    deriving Generic

-- | The body of an interpolated @Text@ literal
data Chunks s a = Chunks [(Text, Expr s a)] Text
    deriving Generic

instance IsString (Chunks s a) where
    fromString str = Chunks [] (fromString str)

-- | Used to record the origin of a @//@ operator (i.e. from source code or a
-- product of desugaring)
data PreferAnnotation s a
    = PreferFromSource
    | PreferFromWith (Expr s a)
      -- ^ Stores the original @with@ expression
    | PreferFromCompletion
    deriving Generic

-- | Record the field of a record-type and record-literal expression.
-- The reason why we use the same ADT for both of them is because they store
-- the same information.
--
-- For example,
--
-- > { {- A -} x {- B -} : {- C -} T }
--
-- ... or
--
-- > { {- A -} x {- B -} = {- C -} T }
--
-- will be instantiated as follows:
--
-- * @recordFieldSrc0@ corresponds to the @A@ comment.
-- * @recordFieldValue@ is @"T"@
-- * @recordFieldSrc1@ corresponds to the @B@ comment.
-- * @recordFieldSrc2@ corresponds to the @C@ comment.
--
-- Although the @A@ comment isn't annotating the @"T"@ Record Field,
-- this is the best place to keep these comments.
--
-- Note that @recordFieldSrc2@ is always 'Nothing' when the 'RecordField' is for
-- a punned entry, because there is no @=@ sign. For example,
--
-- > { {- A -} x {- B -} }
--
-- will be instantiated as follows:
--
-- * @recordFieldSrc0@ corresponds to the @A@ comment.
-- * @recordFieldValue@ corresponds to @(Var "x")@
-- * @recordFieldSrc1@ corresponds to the @B@ comment.
-- * @recordFieldSrc2@ will be 'Nothing'
--
-- The labels involved in a record using dot-syntax like in this example:
--
-- > { {- A -} a {- B -} . {- C -} b {- D -} . {- E -} c {- F -} = {- G -} e }
--
-- will be instantiated as follows:
--
-- * For both the @a@ and @b@ field, @recordfieldSrc2@ is 'Nothing'
-- * For the @a@ field:
--   * @recordFieldSrc0@ corresponds to the @A@ comment
--   * @recordFieldSrc1@ corresponds to the @B@ comment
-- * For the @b@ field:
--   * @recordFieldSrc0@ corresponds to the @C@ comment
--   * @recordFieldSrc1@ corresponds to the @D@ comment
-- * For the @c@ field:
--   * @recordFieldSrc0@ corresponds to the @E@ comment
--   * @recordFieldSrc1@ corresponds to the @F@ comment
--   * @recordFieldSrc2@ corresponds to the @G@ comment
--
-- That is, for every label except the last one the semantics of
-- @recordFieldSrc0@ and @recordFieldSrc1@ are the same from a regular record
-- label but @recordFieldSrc2@ is always 'Nothing'. For the last keyword, all
-- srcs are 'Just'
data RecordField s a = RecordField
    { recordFieldSrc0  :: Maybe s
    , recordFieldValue :: Expr s a
    , recordFieldSrc1  :: Maybe s
    , recordFieldSrc2  :: Maybe s
    } deriving Generic

-- | Construct a 'RecordField' with no src information
makeRecordField :: Expr s a -> RecordField s a
makeRecordField e = RecordField Nothing e Nothing Nothing

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
