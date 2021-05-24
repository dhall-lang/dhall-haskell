{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

{-| Please read the "Dhall.Tutorial" module, which contains a tutorial explaining
    how to use the language, the compiler, and this library
-}

module Dhall.Marshal.Internal
    ( InputNormalizer(..)
    , defaultInputNormalizer
    , InterpretOptions(..)
    , SingletonConstructors(..)
    , defaultInterpretOptions

    -- * Miscellaneous
    , Result(..)

    -- * Helpers for the generic deriving machinery
    , getSelName
    , notEmptyRecord
    , notEmptyRecordLit
    , unsafeExpectRecord
    , unsafeExpectRecordLit
    , unsafeExpectUnion
    , unsafeExpectUnionLit

    -- * Re-exports
    , Fix(..)
    , HashMap
    , Map
    , Natural
    , Scientific
    , Seq
    , Text
    , Vector
    , Void
    , Word8
    , Word16
    , Word32
    , Word64
    , Generic
    ) where

import Control.Monad.Trans.State.Strict
import Data.Fix                             (Fix (..))
import Data.HashMap.Strict                  (HashMap)
import Data.Map                             (Map)
import Data.Scientific                      (Scientific)
import Data.Sequence                        (Seq)
import Data.Text                            (Text)
import Data.Vector                          (Vector)
import Data.Void                            (Void)
import Data.Word                            (Word8, Word16, Word32, Word64)
import Dhall.Parser                         (Src (..))
import Dhall.Syntax
    ( Expr (..)
    , RecordField (..)
    )
import GHC.Generics
import Numeric.Natural                      (Natural)
import Prelude                              hiding (maybe, sequence)

import qualified Data.Text
import qualified Dhall.Core                       as Core
import qualified Dhall.Map

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XRecordWildCards
-- >>> import Data.Word (Word8, Word16, Word32, Word64)
-- >>> import Dhall.Pretty.Internal (prettyExpr)

{-| This type is exactly the same as `Data.Fix.Fix` except with a different
    `FromDhall` instance.  This intermediate type simplifies the implementation
    of the inner loop for the `FromDhall` instance for `Fix`
-}
newtype Result f = Result { _unResult :: f (Result f) }

{-| Use these options to tweak how Dhall derives a generic implementation of
    `FromDhall`
-}
data InterpretOptions = InterpretOptions
    { fieldModifier       :: Text -> Text
    -- ^ Function used to transform Haskell field names into their corresponding
    --   Dhall field names
    , constructorModifier :: Text -> Text
    -- ^ Function used to transform Haskell constructor names into their
    --   corresponding Dhall alternative names
    , singletonConstructors :: SingletonConstructors
    -- ^ Specify how to handle constructors with only one field.  The default is
    --   `Smart`
    }

-- | This is only used by the `FromDhall` instance for functions in order
--   to normalize the function input before marshaling the input into a
--   Dhall expression
newtype InputNormalizer = InputNormalizer
  { getInputNormalizer :: Core.ReifiedNormalizer Void }

-- | Default normalization-related settings (no custom normalization)
defaultInputNormalizer :: InputNormalizer
defaultInputNormalizer = InputNormalizer
 { getInputNormalizer = Core.ReifiedNormalizer (const (pure Nothing)) }

{-| This type specifies how to model a Haskell constructor with 1 field in
    Dhall

    For example, consider the following Haskell datatype definition:

    > data Example = Foo { x :: Double } | Bar Double

    Depending on which option you pick, the corresponding Dhall type could be:

    > < Foo : Double | Bar : Double >                   -- Bare

    > < Foo : { x : Double } | Bar : { _1 : Double } >  -- Wrapped

    > < Foo : { x : Double } | Bar : Double >           -- Smart
-}
data SingletonConstructors
    = Bare
    -- ^ Never wrap the field in a record
    | Wrapped
    -- ^ Always wrap the field in a record
    | Smart
    -- ^ Only fields in a record if they are named

{-| Default interpret options for generics-based instances,
    which you can tweak or override, like this:

> genericAutoWith
>     (defaultInterpretOptions { fieldModifier = Data.Text.Lazy.dropWhile (== '_') })
-}
defaultInterpretOptions :: InterpretOptions
defaultInterpretOptions = InterpretOptions
    { fieldModifier =
          id
    , constructorModifier =
          id
    , singletonConstructors =
          Smart
    }

unsafeExpectUnion
    :: Text -> Expr Src Void -> Dhall.Map.Map Text (Maybe (Expr Src Void))
unsafeExpectUnion _ (Union kts) =
    kts
unsafeExpectUnion name expression =
    Core.internalError
        (name <> ": Unexpected constructor: " <> Core.pretty expression)

unsafeExpectRecord
    :: Text -> Expr Src Void -> Dhall.Map.Map Text (RecordField Src Void)
unsafeExpectRecord _ (Record kts) =
    kts
unsafeExpectRecord name expression =
    Core.internalError
        (name <> ": Unexpected constructor: " <> Core.pretty expression)

unsafeExpectUnionLit
    :: Text
    -> Expr Src Void
    -> (Text, Maybe (Expr Src Void))
unsafeExpectUnionLit _ (Field (Union _) (Core.fieldSelectionLabel -> k)) =
    (k, Nothing)
unsafeExpectUnionLit _ (App (Field (Union _) (Core.fieldSelectionLabel -> k)) v) =
    (k, Just v)
unsafeExpectUnionLit name expression =
    Core.internalError
        (name <> ": Unexpected constructor: " <> Core.pretty expression)

unsafeExpectRecordLit
    :: Text -> Expr Src Void -> Dhall.Map.Map Text (RecordField Src Void)
unsafeExpectRecordLit _ (RecordLit kvs) =
    kvs
unsafeExpectRecordLit name expression =
    Core.internalError
        (name <> ": Unexpected constructor: " <> Core.pretty expression)

notEmptyRecordLit :: Expr s a -> Maybe (Expr s a)
notEmptyRecordLit e = case e of
    RecordLit m | null m -> Nothing
    _                    -> Just e

notEmptyRecord :: Expr s a -> Maybe (Expr s a)
notEmptyRecord e = case e of
    Record m | null m -> Nothing
    _                 -> Just e

getSelName :: Selector s => M1 i s f a -> State Int Text
getSelName n = case selName n of
    "" -> do i <- get
             put (i + 1)
             pure (Data.Text.pack ("_" ++ show i))
    nn -> pure (Data.Text.pack nn)
