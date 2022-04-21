{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-| Please read the "Dhall.Tutorial" module, which contains a tutorial explaining
    how to use the language, the compiler, and this library
-}

module Dhall.Marshal.Decode
    ( -- * General
      Decoder (..)
    , FromDhall(..)
    , Interpret
    , auto

      -- * Building decoders
      -- ** Simple decoders
    , bool
    , unit
    , void
      -- ** Numbers
    , natural
    , word
    , word8
    , word16
    , word32
    , word64
    , integer
    , int
    , int8
    , int16
    , int32
    , int64
    , scientific
    , double
      -- ** Textual
    , string
    , lazyText
    , strictText
      -- ** Time
    , timeOfDay
    , day
    , timeZone
    , localTime
    , zonedTime
    , utcTime
      -- ** Containers
    , maybe
    , pair
    , sequence
    , list
    , vector
    , setFromDistinctList
    , setIgnoringDuplicates
    , hashSetFromDistinctList
    , hashSetIgnoringDuplicates
    , Dhall.Marshal.Decode.map
    , hashMap
    , pairFromMapEntry
      -- ** Functions
    , function
    , functionWith
      -- ** Records
    , RecordDecoder(..)
    , record
    , field
      -- ** Unions
    , UnionDecoder(..)
    , union
    , constructor

      -- * Generic decoding
    , GenericFromDhall(..)
    , GenericFromDhallUnion(..)
    , genericAuto
    , genericAutoWith
    , genericAutoWithInputNormalizer

    -- * Decoding errors
    , DhallErrors(..)
    , showDhallErrors
    , InvalidDecoder(..)
    -- ** Extraction errors
    , ExtractErrors
    , ExtractError(..)
    , Extractor
    , typeError
    , extractError
    , MonadicExtractor
    , toMonadic
    , fromMonadic
    -- ** Typing errors
    , ExpectedTypeErrors
    , ExpectedTypeError(..)
    , Expector

    -- * Miscellaneous
    , InputNormalizer(..)
    , defaultInputNormalizer
    , InterpretOptions(..)
    , SingletonConstructors(..)
    , defaultInterpretOptions
    , Result

    -- * Re-exports
    , Natural
    , Seq
    , Text
    , Vector
    , Generic
    ) where


import Control.Applicative              (empty, liftA2)
import Control.Exception                (Exception)
import Control.Monad                    (guard)
import Control.Monad.Trans.State.Strict
import Data.Coerce                      (coerce)
import Data.Either.Validation
    ( Validation (..)
    , eitherToValidation
    , validationToEither
    )
import Data.Functor.Contravariant
    ( Equivalence (..)
    , Op (..)
    , Predicate (..)
    )
import Data.Hashable                    (Hashable)
import Data.List.NonEmpty               (NonEmpty (..))
import Data.Typeable                    (Proxy (..), Typeable)
import Dhall.Parser                     (Src (..))
import Dhall.Syntax
    ( Chunks (..)
    , DhallDouble (..)
    , Expr (..)
    , FieldSelection (..)
    , FunctionBinding (..)
    , Var (..)
    )
import GHC.Generics
import Prelude                          hiding (maybe, sequence)
import Prettyprinter                    (Pretty)

import qualified Control.Applicative
import qualified Data.Foldable
import qualified Data.Functor.Compose
import qualified Data.Functor.Product
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.HashSet
import qualified Data.List            as List
import qualified Data.List.NonEmpty
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Scientific
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Time            as Time
import qualified Data.Vector
import qualified Dhall.Core           as Core
import qualified Dhall.Map
import qualified Dhall.Util

import Dhall.Marshal.Encode
import Dhall.Marshal.Internal

-- $setup
-- >>> import Dhall (input)

{-| A @(Decoder a)@ represents a way to marshal a value of type @\'a\'@ from Dhall
    into Haskell.

    You can produce `Decoder`s either explicitly:

> example :: Decoder (Vector Text)
> example = vector text

    ... or implicitly using `auto`:

> example :: Decoder (Vector Text)
> example = auto

    You can consume `Decoder`s using the `Dhall.input` function:

> input :: Decoder a -> Text -> IO a
-}
data Decoder a = Decoder
    { extract  :: Expr Src Void -> Extractor Src Void a
    -- ^ Extracts Haskell value from the Dhall expression
    , expected :: Expector (Expr Src Void)
    -- ^ Dhall type of the Haskell value
    }
    deriving (Functor)

{-| Any value that implements `FromDhall` can be automatically decoded based on
    the inferred return type of `Dhall.input`.

>>> input auto "[1, 2, 3]" :: IO (Vector Natural)
[1,2,3]
>>> input auto "toMap { a = False, b = True }" :: IO (Map Text Bool)
fromList [("a",False),("b",True)]

    This class auto-generates a default implementation for types that
    implement `Generic`.  This does not auto-generate an instance for recursive
    types.

    The default instance can be tweaked using 'genericAutoWith'/'genericAutoWithInputNormalizer'
    and custom 'InterpretOptions', or using
    [DerivingVia](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DerivingVia)
    and 'Dhall.Deriving.Codec' from "Dhall.Deriving".
-}
class FromDhall a where
    autoWith :: InputNormalizer -> Decoder a
    default autoWith
        :: (Generic a, GenericFromDhall a (Rep a)) => InputNormalizer -> Decoder a
    autoWith _ = genericAuto

-- | A compatibility alias for `FromDhall`.
type Interpret = FromDhall
{-# DEPRECATED Interpret "Use FromDhall instead" #-}

{-| Use the default input normalizer for interpreting an input.

> auto = autoWith defaultInputNormalizer
-}
auto :: FromDhall a => Decoder a
auto = autoWith defaultInputNormalizer



instance FromDhall Void where
    autoWith _ = void

instance FromDhall () where
    autoWith _ = unit

instance FromDhall Bool where
    autoWith _ = bool

instance FromDhall Natural where
    autoWith _ = natural

instance FromDhall Word where
    autoWith _ = word

instance FromDhall Word8 where
    autoWith _ = word8

instance FromDhall Word16 where
    autoWith _ = word16

instance FromDhall Word32 where
    autoWith _ = word32

instance FromDhall Word64 where
    autoWith _ = word64

instance FromDhall Integer where
    autoWith _ = integer

instance FromDhall Int where
    autoWith _ = int

instance FromDhall Int8 where
    autoWith _ = int8

instance FromDhall Int16 where
    autoWith _ = int16

instance FromDhall Int32 where
    autoWith _ = int32

instance FromDhall Int64 where
    autoWith _ = int64

instance FromDhall Scientific where
    autoWith _ = scientific

instance FromDhall Double where
    autoWith _ = double

instance {-# OVERLAPS #-} FromDhall [Char] where
    autoWith _ = string

instance FromDhall Data.Text.Lazy.Text where
    autoWith _ = lazyText

instance FromDhall Text where
    autoWith _ = strictText

instance FromDhall a => FromDhall (Maybe a) where
    autoWith opts = maybe (autoWith opts)

instance FromDhall a => FromDhall (Seq a) where
    autoWith opts = sequence (autoWith opts)

instance FromDhall a => FromDhall [a] where
    autoWith opts = list (autoWith opts)

instance FromDhall a => FromDhall (Vector a) where
    autoWith opts = vector (autoWith opts)

instance FromDhall Time.TimeOfDay where
    autoWith _ = timeOfDay

instance FromDhall Time.Day where
    autoWith _ = day

instance FromDhall Time.TimeZone where
    autoWith _ = timeZone

instance FromDhall Time.LocalTime where
    autoWith _ = localTime

instance FromDhall Time.ZonedTime where
    autoWith _ = zonedTime

instance FromDhall Time.UTCTime where
    autoWith _ = utcTime

instance FromDhall Time.DayOfWeek where
    autoWith _ = dayOfWeek

{-| Note that this instance will throw errors in the presence of duplicates in
    the list. To ignore duplicates, use `setIgnoringDuplicates`.
-}
instance (FromDhall a, Ord a, Show a) => FromDhall (Data.Set.Set a) where
    autoWith opts = setFromDistinctList (autoWith opts)

{-| Note that this instance will throw errors in the presence of duplicates in
    the list. To ignore duplicates, use `hashSetIgnoringDuplicates`.
-}
instance (FromDhall a, Hashable a, Ord a, Show a) => FromDhall (Data.HashSet.HashSet a) where
    autoWith inputNormalizer = hashSetFromDistinctList (autoWith inputNormalizer)

instance (Ord k, FromDhall k, FromDhall v) => FromDhall (Map k v) where
    autoWith inputNormalizer = Dhall.Marshal.Decode.map (autoWith inputNormalizer) (autoWith inputNormalizer)

instance (Eq k, Hashable k, FromDhall k, FromDhall v) => FromDhall (HashMap k v) where
    autoWith inputNormalizer = Dhall.Marshal.Decode.hashMap (autoWith inputNormalizer) (autoWith inputNormalizer)

instance (ToDhall a, FromDhall b) => FromDhall (a -> b) where
    autoWith inputNormalizer =
        functionWith inputNormalizer (injectWith inputNormalizer) (autoWith inputNormalizer)

instance (FromDhall a, FromDhall b) => FromDhall (a, b)

instance FromDhall (f (Result f)) => FromDhall (Result f) where
    autoWith inputNormalizer = Decoder {..}
      where
        extract (App _ expr) =
            fmap Result (Dhall.Marshal.Decode.extract (autoWith inputNormalizer) expr)
        extract expr = typeError expected expr

        expected = pure "result"

deriving newtype instance (ToDhall x) => FromDhall (Predicate x)

deriving newtype instance (ToDhall x) => FromDhall (Equivalence x)

deriving newtype instance (FromDhall b, ToDhall x) => FromDhall (Op b x)

-- | You can use this instance to marshal recursive types from Dhall to Haskell.
--
-- Here is an example use of this instance:
--
-- > {-# LANGUAGE DeriveAnyClass     #-}
-- > {-# LANGUAGE DeriveFoldable     #-}
-- > {-# LANGUAGE DeriveFunctor      #-}
-- > {-# LANGUAGE DeriveTraversable  #-}
-- > {-# LANGUAGE DeriveGeneric      #-}
-- > {-# LANGUAGE KindSignatures     #-}
-- > {-# LANGUAGE QuasiQuotes        #-}
-- > {-# LANGUAGE StandaloneDeriving #-}
-- > {-# LANGUAGE TypeFamilies       #-}
-- > {-# LANGUAGE TemplateHaskell    #-}
-- >
-- > import Data.Fix (Fix(..))
-- > import Data.Text (Text)
-- > import Dhall (FromDhall)
-- > import GHC.Generics (Generic)
-- > import Numeric.Natural (Natural)
-- >
-- > import qualified Data.Fix                 as Fix
-- > import qualified Data.Functor.Foldable    as Foldable
-- > import qualified Data.Functor.Foldable.TH as TH
-- > import qualified Dhall
-- > import qualified NeatInterpolation
-- >
-- > data Expr
-- >     = Lit Natural
-- >     | Add Expr Expr
-- >     | Mul Expr Expr
-- >     deriving (Show)
-- >
-- > TH.makeBaseFunctor ''Expr
-- >
-- > deriving instance Generic (ExprF a)
-- > deriving instance FromDhall a => FromDhall (ExprF a)
-- >
-- > example :: Text
-- > example = [NeatInterpolation.text|
-- >     \(Expr : Type)
-- > ->  let ExprF =
-- >           < LitF :
-- >               Natural
-- >           | AddF :
-- >               { _1 : Expr, _2 : Expr }
-- >           | MulF :
-- >               { _1 : Expr, _2 : Expr }
-- >           >
-- >
-- >     in      \(Fix : ExprF -> Expr)
-- >         ->  let Lit = \(x : Natural) -> Fix (ExprF.LitF x)
-- >
-- >             let Add =
-- >                       \(x : Expr)
-- >                   ->  \(y : Expr)
-- >                   ->  Fix (ExprF.AddF { _1 = x, _2 = y })
-- >
-- >             let Mul =
-- >                       \(x : Expr)
-- >                   ->  \(y : Expr)
-- >                   ->  Fix (ExprF.MulF { _1 = x, _2 = y })
-- >
-- >             in  Add (Mul (Lit 3) (Lit 7)) (Add (Lit 1) (Lit 2))
-- > |]
-- >
-- > convert :: Fix ExprF -> Expr
-- > convert = Fix.foldFix Foldable.embed
-- >
-- > main :: IO ()
-- > main = do
-- >     x <- Dhall.input Dhall.auto example :: IO (Fix ExprF)
-- >
-- >     print (convert x :: Expr)
instance (Functor f, FromDhall (f (Result f))) => FromDhall (Fix f) where
    autoWith inputNormalizer = Decoder {..}
      where
        extract expr0 = extract0 expr0
          where
            die = typeError expected expr0

            extract0 (Lam _ (FunctionBinding { functionBindingVariable = x }) expr) =
                extract1 (rename x "result" expr)
            extract0  _             = die

            extract1 (Lam _ (FunctionBinding { functionBindingVariable = y }) expr) =
                extract2 (rename y "Make" expr)
            extract1  _             = die

            extract2 expr = fmap resultToFix (Dhall.Marshal.Decode.extract (autoWith inputNormalizer) expr)

            rename a b expr
                | a /= b    = Core.subst (V a 0) (Var (V b 0)) (Core.shift 1 (V b 0) expr)
                | otherwise = expr

        expected = (\x -> Pi mempty "result" (Const Core.Type) (Pi mempty "Make" (Pi mempty "_" x "result") "result"))
            <$> Dhall.Marshal.Decode.expected (autoWith inputNormalizer :: Decoder (f (Result f)))

resultToFix :: Functor f => Result f -> Fix f
resultToFix (Result x) = Fix (fmap resultToFix x)



{-| This is the underlying class that powers the `FromDhall` class's support
    for automatically deriving a generic implementation.
-}
class GenericFromDhall t f where
    genericAutoWithNormalizer :: Proxy t -> InputNormalizer -> InterpretOptions -> State Int (Decoder (f a))

instance GenericFromDhall t f => GenericFromDhall t (M1 D d f) where
    genericAutoWithNormalizer p inputNormalizer options = do
        res <- genericAutoWithNormalizer p inputNormalizer options
        pure (fmap M1 res)

instance GenericFromDhall t V1 where
    genericAutoWithNormalizer _ _ _ = pure Decoder {..}
      where
        extract expr = typeError expected expr

        expected = pure $ Union mempty

instance GenericFromDhallUnion t (f :+: g) => GenericFromDhall t (f :+: g) where
  genericAutoWithNormalizer p inputNormalizer options =
    pure (union (genericUnionAutoWithNormalizer p inputNormalizer options))

instance GenericFromDhall t f => GenericFromDhall t (M1 C c f) where
    genericAutoWithNormalizer p inputNormalizer options = do
        res <- genericAutoWithNormalizer p inputNormalizer options
        pure (fmap M1 res)

instance GenericFromDhall t U1 where
    genericAutoWithNormalizer _ _ _ = pure (Decoder {..})
      where
        extract _ = pure U1

        expected = pure expected'

        expected' = Record (Dhall.Map.fromList [])

instance (GenericFromDhall t (f :*: g), GenericFromDhall t (h :*: i)) => GenericFromDhall t ((f :*: g) :*: (h :*: i)) where
    genericAutoWithNormalizer p inputNormalizer options = do
        Decoder extractL expectedL <- genericAutoWithNormalizer p inputNormalizer options
        Decoder extractR expectedR <- genericAutoWithNormalizer p inputNormalizer options

        let ktsL = unsafeExpectRecord "genericAutoWithNormalizer (:*:)" <$> expectedL
        let ktsR = unsafeExpectRecord "genericAutoWithNormalizer (:*:)" <$> expectedR

        let expected = Record <$> (Dhall.Map.union <$> ktsL <*> ktsR)

        let extract expression =
                liftA2 (:*:) (extractL expression) (extractR expression)

        return (Decoder {..})

instance (GenericFromDhall t (f :*: g), Selector s, FromDhall a) => GenericFromDhall t ((f :*: g) :*: M1 S s (K1 i a)) where
    genericAutoWithNormalizer p inputNormalizer options@InterpretOptions{..} = do
        let nR :: M1 S s (K1 i a) r
            nR = undefined

        nameR <- fmap fieldModifier (getSelName nR)

        Decoder extractL expectedL <- genericAutoWithNormalizer p inputNormalizer options

        let Decoder extractR expectedR = autoWith inputNormalizer

        let ktsL = unsafeExpectRecord "genericAutoWithNormalizer (:*:)" <$> expectedL

        let expected = Record <$> (Dhall.Map.insert nameR . Core.makeRecordField <$> expectedR <*> ktsL)

        let extract expression = do
                let die = typeError expected expression

                case expression of
                    RecordLit kvs ->
                        case Core.recordFieldValue <$> Dhall.Map.lookup nameR kvs of
                            Just expressionR ->
                                liftA2 (:*:)
                                    (extractL expression)
                                    (fmap (M1 . K1) (extractR expressionR))
                            _ -> die
                    _ -> die

        return (Decoder {..})

instance (Selector s, FromDhall a, GenericFromDhall t (f :*: g)) => GenericFromDhall t (M1 S s (K1 i a) :*: (f :*: g)) where
    genericAutoWithNormalizer p inputNormalizer options@InterpretOptions{..} = do
        let nL :: M1 S s (K1 i a) r
            nL = undefined

        nameL <- fmap fieldModifier (getSelName nL)

        let Decoder extractL expectedL = autoWith inputNormalizer

        Decoder extractR expectedR <- genericAutoWithNormalizer p inputNormalizer options

        let ktsR = unsafeExpectRecord "genericAutoWithNormalizer (:*:)" <$> expectedR

        let expected = Record <$> (Dhall.Map.insert nameL . Core.makeRecordField <$> expectedL <*> ktsR)

        let extract expression = do
                let die = typeError expected expression

                case expression of
                    RecordLit kvs ->
                        case Core.recordFieldValue <$> Dhall.Map.lookup nameL kvs of
                            Just expressionL ->
                                liftA2 (:*:)
                                    (fmap (M1 . K1) (extractL expressionL))
                                    (extractR expression)
                            _ -> die
                    _ -> die

        return (Decoder {..})

instance {-# OVERLAPPING #-} GenericFromDhall a1 (M1 S s1 (K1 i1 a1) :*: M1 S s2 (K1 i2 a2)) where
    genericAutoWithNormalizer _ _ _ = pure $ Decoder
        { extract = \_ -> Failure $ DhallErrors $ pure $ ExpectedTypeError RecursiveTypeError
        , expected = Failure $ DhallErrors $ pure RecursiveTypeError
        }

instance {-# OVERLAPPING #-} GenericFromDhall a2 (M1 S s1 (K1 i1 a1) :*: M1 S s2 (K1 i2 a2)) where
    genericAutoWithNormalizer _ _ _ = pure $ Decoder
        { extract = \_ -> Failure $ DhallErrors $ pure $ ExpectedTypeError RecursiveTypeError
        , expected = Failure $ DhallErrors $ pure RecursiveTypeError
        }

instance {-# OVERLAPPABLE #-} (Selector s1, Selector s2, FromDhall a1, FromDhall a2) => GenericFromDhall t (M1 S s1 (K1 i1 a1) :*: M1 S s2 (K1 i2 a2)) where
    genericAutoWithNormalizer _ inputNormalizer InterpretOptions{..} = do
        let nL :: M1 S s1 (K1 i1 a1) r
            nL = undefined

        let nR :: M1 S s2 (K1 i2 a2) r
            nR = undefined

        nameL <- fmap fieldModifier (getSelName nL)
        nameR <- fmap fieldModifier (getSelName nR)

        let Decoder extractL expectedL = autoWith inputNormalizer
        let Decoder extractR expectedR = autoWith inputNormalizer

        let expected = do
                l <- Core.makeRecordField <$> expectedL
                r <- Core.makeRecordField <$> expectedR
                pure $ Record
                    (Dhall.Map.fromList
                        [ (nameL, l)
                        , (nameR, r)
                        ]
                    )

        let extract expression = do
                let die = typeError expected expression

                case expression of
                    RecordLit kvs ->
                        case liftA2 (,) (Dhall.Map.lookup nameL kvs) (Dhall.Map.lookup nameR kvs) of
                            Just (expressionL, expressionR) ->
                                liftA2 (:*:)
                                    (fmap (M1 . K1) (extractL $ Core.recordFieldValue expressionL))
                                    (fmap (M1 . K1) (extractR $ Core.recordFieldValue expressionR))
                            Nothing -> die
                    _ -> die

        return (Decoder {..})

instance {-# OVERLAPPING #-} GenericFromDhall a (M1 S s (K1 i a)) where
    genericAutoWithNormalizer _ _ _ = pure $ Decoder
        { extract = \_ -> Failure $ DhallErrors $ pure $ ExpectedTypeError RecursiveTypeError
        , expected = Failure $ DhallErrors $ pure RecursiveTypeError
        }

instance {-# OVERLAPPABLE #-} (Selector s, FromDhall a) => GenericFromDhall t (M1 S s (K1 i a)) where
    genericAutoWithNormalizer _ inputNormalizer InterpretOptions{..} = do
        let n :: M1 S s (K1 i a) r
            n = undefined

        name <- fmap fieldModifier (getSelName n)

        let Decoder { extract = extract', expected = expected'} = autoWith inputNormalizer

        let expected =
                case singletonConstructors of
                    Bare ->
                        expected'
                    Smart | selName n == "" ->
                        expected'
                    _ ->
                        Record . Dhall.Map.singleton name . Core.makeRecordField <$> expected'

        let extract0 expression = fmap (M1 . K1) (extract' expression)

        let extract1 expression = do
                let die = typeError expected expression

                case expression of
                    RecordLit kvs ->
                        case Core.recordFieldValue <$> Dhall.Map.lookup name kvs of
                            Just subExpression ->
                                fmap (M1 . K1) (extract' subExpression)
                            Nothing ->
                                die
                    _ -> die

        let extract =
                case singletonConstructors of
                    Bare                    -> extract0
                    Smart | selName n == "" -> extract0
                    _                       -> extract1

        return (Decoder {..})

{-| `genericAuto` is the default implementation for `auto` if you derive
    `FromDhall`.  The difference is that you can use `genericAuto` without
    having to explicitly provide a `FromDhall` instance for a type as long as
    the type derives `Generic`.
-}
genericAuto :: (Generic a, GenericFromDhall a (Rep a)) => Decoder a
genericAuto = genericAutoWith defaultInterpretOptions

{-| `genericAutoWith` is a configurable version of `genericAuto`.
-}
genericAutoWith :: (Generic a, GenericFromDhall a (Rep a)) => InterpretOptions -> Decoder a
genericAutoWith options = genericAutoWithInputNormalizer options defaultInputNormalizer

{-| `genericAutoWithInputNormalizer` is like `genericAutoWith`, but instead of
    using the `defaultInputNormalizer` it expects an custom `InputNormalizer`.
-}
genericAutoWithInputNormalizer :: (Generic a, GenericFromDhall a (Rep a)) => InterpretOptions -> InputNormalizer -> Decoder a
genericAutoWithInputNormalizer options inputNormalizer = withProxy (\p -> fmap to (evalState (genericAutoWithNormalizer p inputNormalizer options) 1))
    where
        withProxy :: (Proxy a -> Decoder a) -> Decoder a
        withProxy f = f Proxy

extractUnionConstructor
    :: Expr s a -> Maybe (Text, Expr s a, Dhall.Map.Map Text (Maybe (Expr s a)))
extractUnionConstructor (App (Field (Union kts) (Core.fieldSelectionLabel -> fld)) e) =
  return (fld, e, Dhall.Map.delete fld kts)
extractUnionConstructor (Field (Union kts) (Core.fieldSelectionLabel -> fld)) =
  return (fld, RecordLit mempty, Dhall.Map.delete fld kts)
extractUnionConstructor _ =
  empty

{-| This is the underlying class that powers the `FromDhall` class's support
    for automatically deriving a generic implementation for a union type.
-}
class GenericFromDhallUnion t f where
    genericUnionAutoWithNormalizer :: Proxy t -> InputNormalizer -> InterpretOptions -> UnionDecoder (f a)

instance (GenericFromDhallUnion t f1, GenericFromDhallUnion t f2) => GenericFromDhallUnion t (f1 :+: f2) where
  genericUnionAutoWithNormalizer p inputNormalizer options =
    (<>)
      (L1 <$> genericUnionAutoWithNormalizer p inputNormalizer options)
      (R1 <$> genericUnionAutoWithNormalizer p inputNormalizer options)

instance (Constructor c1, GenericFromDhall t f1) => GenericFromDhallUnion t (M1 C c1 f1) where
  genericUnionAutoWithNormalizer p inputNormalizer options@(InterpretOptions {..}) =
    constructor name (evalState (genericAutoWithNormalizer p inputNormalizer options) 1)
    where
      n :: M1 C c1 f1 a
      n = undefined

      name = constructorModifier (Data.Text.pack (conName n))



{-| Decode a `Prelude.Bool`.

>>> input bool "True"
True
-}
bool :: Decoder Bool
bool = Decoder {..}
  where
    extract (BoolLit b) = pure b
    extract expr        = typeError expected expr

    expected = pure Bool

{-| Decode a `Prelude.Natural`.

>>> input natural "42"
42
-}
natural :: Decoder Natural
natural = Decoder {..}
  where
    extract (NaturalLit n) = pure n
    extract  expr          = typeError expected expr

    expected = pure Natural

{-| Decode an `Prelude.Integer`.

>>> input integer "+42"
42
-}
integer :: Decoder Integer
integer = Decoder {..}
  where
    extract (IntegerLit n) = pure n
    extract  expr          = typeError expected expr

    expected = pure Integer

wordHelper :: forall a . (Bounded a, Integral a) => Text -> Decoder a
wordHelper name = Decoder {..}
  where
    extract (NaturalLit n)
        | toInteger n <= toInteger (maxBound @a) =
            pure (fromIntegral n)
        | otherwise =
            extractError ("Decoded " <> name <> " is out of bounds: " <> Data.Text.pack (show n))
    extract expr =
        typeError expected expr

    expected = pure Natural

{-| Decode a `Word` from a Dhall @Natural@.

>>> input word "42"
42
-}
word :: Decoder Word
word = wordHelper "Word"

{-| Decode a `Word8` from a Dhall @Natural@.

>>> input word8 "42"
42
-}
word8 :: Decoder Word8
word8 = wordHelper "Word8"

{-| Decode a `Word16` from a Dhall @Natural@.

>>> input word16 "42"
42
-}
word16 :: Decoder Word16
word16 = wordHelper "Word16"

{-| Decode a `Word32` from a Dhall @Natural@.

>>> input word32 "42"
42
-}
word32 :: Decoder Word32
word32 = wordHelper "Word32"

{-| Decode a `Word64` from a Dhall @Natural@.

>>> input word64 "42"
42
-}
word64 :: Decoder Word64
word64 = wordHelper "Word64"

intHelper :: forall a . (Bounded a, Integral a) => Text -> Decoder a
intHelper name = Decoder {..}
  where
    extract (IntegerLit n)
        | toInteger (minBound @a) <= n && n <= toInteger (maxBound @a) =
            pure (fromIntegral n)
        | otherwise =
            extractError ("Decoded " <> name <> " is out of bounds: " <> Data.Text.pack (show n))
    extract expr =
        typeError expected expr

    expected = pure Integer

{-| Decode an `Int` from a Dhall @Integer@.

>>> input int "-42"
-42
-}
int :: Decoder Int
int = intHelper "Int"

{-| Decode an `Int8` from a Dhall @Integer@.

>>> input int8 "-42"
-42
-}
int8 :: Decoder Int8
int8 = intHelper "Int8"

{-| Decode an `Int16` from a Dhall @Integer@.

>>> input int16 "-42"
-42
-}
int16 :: Decoder Int16
int16 = intHelper "Int16"

{-| Decode an `Int32` from a Dhall @Integer@.

>>> input int32 "-42"
-42
-}
int32 :: Decoder Int32
int32 = intHelper "Int32"

{-| Decode an `Int64` from a Dhall @Integer@.

>>> input int64 "-42"
-42
-}
int64 :: Decoder Int64
int64 = intHelper "Int64"

{-| Decode a `Scientific`.

>>> input scientific "1e100"
1.0e100
-}
scientific :: Decoder Scientific
scientific = fmap Data.Scientific.fromFloatDigits double

{-| Decode a `Prelude.Double`.

>>> input double "42.0"
42.0
-}
double :: Decoder Double
double = Decoder {..}
  where
    extract (DoubleLit (DhallDouble n)) = pure n
    extract  expr                       = typeError expected expr

    expected = pure Double

{-| Decode lazy `Data.Text.Text`.

>>> input lazyText "\"Test\""
"Test"
-}
lazyText :: Decoder Data.Text.Lazy.Text
lazyText = fmap Data.Text.Lazy.fromStrict strictText

{-| Decode strict `Data.Text.Text`.

>>> input strictText "\"Test\""
"Test"
-}
strictText :: Decoder Text
strictText = Decoder {..}
  where
    extract (TextLit (Chunks [] t)) = pure t
    extract  expr                   = typeError expected expr

    expected = pure Text

{-| Decode `Time.TimeOfDay`

>>> input timeOfDay "00:00:00"
00:00:00
-}
timeOfDay :: Decoder Time.TimeOfDay
timeOfDay = Decoder {..}
  where
    extract (TimeLiteral t _) = pure t
    extract  expr             = typeError expected expr

    expected = pure Time

{-| Decode `Time.Day`

>>> input day "2000-01-01"
2000-01-01
-}
day :: Decoder Time.Day
day = Decoder {..}
  where
    extract (DateLiteral d) = pure d
    extract  expr           = typeError expected expr

    expected = pure Date

{-| Decode `Time.TimeZone`

>>> input timeZone "+00:00"
+0000
-}
timeZone :: Decoder Time.TimeZone
timeZone = Decoder {..}
  where
    extract (TimeZoneLiteral z) = pure z
    extract  expr               = typeError expected expr

    expected = pure TimeZone

{-| Decode `Time.LocalTime`

>>> input localTime "2020-01-01T12:34:56"
2020-01-01 12:34:56
-}
localTime :: Decoder Time.LocalTime
localTime = record $
  Time.LocalTime
    <$> field "date" day
    <*> field "time" timeOfDay

{-| Decode `Time.ZonedTime`

>>> input zonedTime "2020-01-01T12:34:56+02:00"
2020-01-01 12:34:56 +0200
-}
zonedTime :: Decoder Time.ZonedTime
zonedTime = record $
  adapt
    <$> field "date" day
    <*> field "time" timeOfDay
    <*> field "timeZone" timeZone
  where
    adapt date time = Time.ZonedTime (Time.LocalTime date time)

{-| Decode `Time.UTCTime`

>>> input utcTime "2020-01-01T12:34:56+02:00"
2020-01-01 10:34:56 UTC
-}
utcTime :: Decoder Time.UTCTime
utcTime = Time.zonedTimeToUTC <$> zonedTime

dayOfWeek :: Decoder Time.DayOfWeek
dayOfWeek = Decoder{..}
  where
    extract expr@(Field _ FieldSelection{ fieldSelectionLabel }) =
        case fieldSelectionLabel of
            "Sunday"    -> pure Time.Sunday
            "Monday"    -> pure Time.Monday
            "Tuesday"   -> pure Time.Tuesday
            "Wednesday" -> pure Time.Wednesday
            "Thursday"  -> pure Time.Thursday
            "Friday"    -> pure Time.Friday
            "Saturday"  -> pure Time.Saturday
            _           -> typeError expected expr
    extract expr =
        typeError expected expr

    expected =
        pure
            (Union
                (Dhall.Map.fromList
                    [ ("Sunday", Nothing)
                    , ("Monday", Nothing)
                    , ("Tuesday", Nothing)
                    , ("Wednesday", Nothing)
                    , ("Thursday", Nothing)
                    , ("Friday", Nothing)
                    , ("Saturday", Nothing)
                    ]
                )
            )

{-| Decode a `Maybe`.

>>> input (maybe natural) "Some 1"
Just 1
-}
maybe :: Decoder a -> Decoder (Maybe a)
maybe (Decoder extractIn expectedIn) = Decoder extractOut expectedOut
  where
    extractOut (Some e    ) = fmap Just (extractIn e)
    extractOut (App None _) = pure Nothing
    extractOut expr         = typeError expectedOut expr

    expectedOut = App Optional <$> expectedIn

{-| Decode a `Seq`.

>>> input (sequence natural) "[1, 2, 3]"
fromList [1,2,3]
-}
sequence :: Decoder a -> Decoder (Seq a)
sequence (Decoder extractIn expectedIn) = Decoder extractOut expectedOut
  where
    extractOut (ListLit _ es) = traverse extractIn es
    extractOut expr           = typeError expectedOut expr

    expectedOut = App List <$> expectedIn

{-| Decode a list.

>>> input (list natural) "[1, 2, 3]"
[1,2,3]
-}
list :: Decoder a -> Decoder [a]
list = fmap Data.Foldable.toList . sequence

{-| Decode a `Vector`.

>>> input (vector natural) "[1, 2, 3]"
[1,2,3]
-}
vector :: Decoder a -> Decoder (Vector a)
vector = fmap Data.Vector.fromList . list

{-| Decode a Dhall function into a Haskell function.

>>> f <- input (function inject bool) "Natural/even" :: IO (Natural -> Bool)
>>> f 0
True
>>> f 1
False
-}
function
    :: Encoder a
    -> Decoder b
    -> Decoder (a -> b)
function = functionWith defaultInputNormalizer

{-| Decode a Dhall function into a Haskell function using the specified normalizer.

>>> f <- input (functionWith defaultInputNormalizer inject bool) "Natural/even" :: IO (Natural -> Bool)
>>> f 0
True
>>> f 1
False
-}
functionWith
    :: InputNormalizer
    -> Encoder a
    -> Decoder b
    -> Decoder (a -> b)
functionWith inputNormalizer (Encoder {..}) (Decoder extractIn expectedIn) =
    Decoder extractOut expectedOut
  where
    normalizer_ = Just (getInputNormalizer inputNormalizer)

    extractOut e = pure (\i -> case extractIn (Core.normalizeWith normalizer_ (App e (embed i))) of
        Success o  -> o
        Failure _e -> error "FromDhall: You cannot decode a function if it does not have the correct type" )

    expectedOut = Pi mempty "_" declared <$> expectedIn

{-| Decode a `Data.Set.Set` from a `List`.

>>> input (setIgnoringDuplicates natural) "[1, 2, 3]"
fromList [1,2,3]

Duplicate elements are ignored.

>>> input (setIgnoringDuplicates natural) "[1, 1, 3]"
fromList [1,3]

-}
setIgnoringDuplicates :: (Ord a) => Decoder a -> Decoder (Data.Set.Set a)
setIgnoringDuplicates = fmap Data.Set.fromList . list

{-| Decode a `Data.HashSet.HashSet` from a `List`.

>>> input (hashSetIgnoringDuplicates natural) "[1, 2, 3]"
fromList [1,2,3]

Duplicate elements are ignored.

>>> input (hashSetIgnoringDuplicates natural) "[1, 1, 3]"
fromList [1,3]

-}
hashSetIgnoringDuplicates :: (Hashable a, Ord a)
                          => Decoder a
                          -> Decoder (Data.HashSet.HashSet a)
hashSetIgnoringDuplicates = fmap Data.HashSet.fromList . list

{-| Decode a `Data.Set.Set` from a `List` with distinct elements.

>>> input (setFromDistinctList natural) "[1, 2, 3]"
fromList [1,2,3]

An error is thrown if the list contains duplicates.

> >>> input (setFromDistinctList natural) "[1, 1, 3]"
> *** Exception: Error: Failed extraction
>
> The expression type-checked successfully but the transformation to the target
> type failed with the following error:
>
> One duplicate element in the list: 1
>

> >>> input (setFromDistinctList natural) "[1, 1, 3, 3]"
> *** Exception: Error: Failed extraction
>
> The expression type-checked successfully but the transformation to the target
> type failed with the following error:
>
> 2 duplicates were found in the list, including 1
>

-}
setFromDistinctList :: (Ord a, Show a) => Decoder a -> Decoder (Data.Set.Set a)
setFromDistinctList = setHelper Data.Set.size Data.Set.fromList

{-| Decode a `Data.HashSet.HashSet` from a `List` with distinct elements.

>>> input (hashSetFromDistinctList natural) "[1, 2, 3]"
fromList [1,2,3]

An error is thrown if the list contains duplicates.

> >>> input (hashSetFromDistinctList natural) "[1, 1, 3]"
> *** Exception: Error: Failed extraction
>
> The expression type-checked successfully but the transformation to the target
> type failed with the following error:
>
> One duplicate element in the list: 1
>

> >>> input (hashSetFromDistinctList natural) "[1, 1, 3, 3]"
> *** Exception: Error: Failed extraction
>
> The expression type-checked successfully but the transformation to the target
> type failed with the following error:
>
> 2 duplicates were found in the list, including 1
>

-}
hashSetFromDistinctList :: (Hashable a, Ord a, Show a)
                        => Decoder a
                        -> Decoder (Data.HashSet.HashSet a)
hashSetFromDistinctList = setHelper Data.HashSet.size Data.HashSet.fromList


setHelper :: (Eq a, Foldable t, Show a)
          => (t a -> Int)
          -> ([a] -> t a)
          -> Decoder a
          -> Decoder (t a)
setHelper size toSet (Decoder extractIn expectedIn) = Decoder extractOut expectedOut
  where
    extractOut (ListLit _ es) = case traverse extractIn es of
        Success vSeq
            | sameSize               -> Success vSet
            | otherwise              -> extractError err
          where
            vList = Data.Foldable.toList vSeq
            vSet = toSet vList
            sameSize = size vSet == Data.Sequence.length vSeq
            duplicates = vList List.\\ Data.Foldable.toList vSet
            err | length duplicates == 1 =
                     "One duplicate element in the list: "
                     <> (Data.Text.pack $ show $ head duplicates)
                | otherwise              = Data.Text.pack $ unwords
                     [ show $ length duplicates
                     , "duplicates were found in the list, including"
                     , show $ head duplicates
                     ]
        Failure f -> Failure f
    extractOut expr = typeError expectedOut expr

    expectedOut = App List <$> expectedIn

{-| Decode a `Map` from a @toMap@ expression or generally a @Prelude.Map.Type@.

>>> input (Dhall.map strictText bool) "toMap { a = True, b = False }"
fromList [("a",True),("b",False)]
>>> input (Dhall.map strictText bool) "[ { mapKey = \"foo\", mapValue = True } ]"
fromList [("foo",True)]

If there are duplicate @mapKey@s, later @mapValue@s take precedence:

>>> let expr = "[ { mapKey = 1, mapValue = True }, { mapKey = 1, mapValue = False } ]"
>>> input (Dhall.map natural bool) expr
fromList [(1,False)]

-}
map :: Ord k => Decoder k -> Decoder v -> Decoder (Map k v)
map k v = fmap Data.Map.fromList (list (pairFromMapEntry k v))

{-| Decode a `HashMap` from a @toMap@ expression or generally a @Prelude.Map.Type@.

>>> fmap (List.sort . HashMap.toList) (input (Dhall.hashMap strictText bool) "toMap { a = True, b = False }")
[("a",True),("b",False)]
>>> fmap (List.sort . HashMap.toList) (input (Dhall.hashMap strictText bool) "[ { mapKey = \"foo\", mapValue = True } ]")
[("foo",True)]

If there are duplicate @mapKey@s, later @mapValue@s take precedence:

>>> let expr = "[ { mapKey = 1, mapValue = True }, { mapKey = 1, mapValue = False } ]"
>>> input (Dhall.hashMap natural bool) expr
fromList [(1,False)]

-}
hashMap :: (Eq k, Hashable k) => Decoder k -> Decoder v -> Decoder (HashMap k v)
hashMap k v = fmap HashMap.fromList (list (pairFromMapEntry k v))

{-| Decode a tuple from a @Prelude.Map.Entry@ record.

>>> input (pairFromMapEntry strictText natural) "{ mapKey = \"foo\", mapValue = 3 }"
("foo",3)
-}
pairFromMapEntry :: Decoder k -> Decoder v -> Decoder (k, v)
pairFromMapEntry k v = Decoder extractOut expectedOut
  where
    extractOut (RecordLit kvs)
        | Just key <- Core.recordFieldValue <$> Dhall.Map.lookup "mapKey" kvs
        , Just value <- Core.recordFieldValue <$> Dhall.Map.lookup "mapValue" kvs
            = liftA2 (,) (extract k key) (extract v value)
    extractOut expr = typeError expectedOut expr

    expectedOut = do
        k' <- Core.makeRecordField <$> expected k
        v' <- Core.makeRecordField <$> expected v
        pure $ Record $ Dhall.Map.fromList
            [ ("mapKey", k')
            , ("mapValue", v')]

{-| Decode @()@ from an empty record.

>>> input unit "{=}"  -- GHC doesn't print the result if it is ()

-}
unit :: Decoder ()
unit = Decoder {..}
  where
    extract (RecordLit fields)
        | Data.Foldable.null fields = pure ()
    extract expr = typeError expected expr

    expected = pure $ Record mempty

{-| Decode 'Void' from an empty union.

Since @<>@ is uninhabited, @'Dhall.input' 'void'@ will always fail.
-}
void :: Decoder Void
void = union mempty

{-| Decode a `String`

>>> input string "\"ABC\""
"ABC"

-}
string :: Decoder String
string = Data.Text.Lazy.unpack <$> lazyText

{-| Given a pair of `Decoder`s, decode a tuple-record into their pairing.

>>> input (pair natural bool) "{ _1 = 42, _2 = False }"
(42,False)
-}
pair :: Decoder a -> Decoder b -> Decoder (a, b)
pair l r = Decoder extractOut expectedOut
  where
    extractOut expr@(RecordLit fields) =
      (,) <$> Data.Maybe.maybe (typeError expectedOut expr) (extract l)
                (Core.recordFieldValue <$> Dhall.Map.lookup "_1" fields)
          <*> Data.Maybe.maybe (typeError expectedOut expr) (extract r)
                (Core.recordFieldValue <$> Dhall.Map.lookup "_2" fields)
    extractOut expr = typeError expectedOut expr

    expectedOut = do
        l' <- Core.makeRecordField <$> expected l
        r' <- Core.makeRecordField <$> expected r
        pure $ Record $ Dhall.Map.fromList
            [ ("_1", l')
            , ("_2", r')]



{-| The 'RecordDecoder' applicative functor allows you to build a 'Decoder'
    from a Dhall record.

    For example, let's take the following Haskell data type:

>>> :{
data Project = Project
  { projectName :: Text
  , projectDescription :: Text
  , projectStars :: Natural
  }
:}

    And assume that we have the following Dhall record that we would like to
    parse as a @Project@:

> { name =
>     "dhall-haskell"
> , description =
>     "A configuration language guaranteed to terminate"
> , stars =
>     289
> }

    Our decoder has type 'Decoder' @Project@, but we can't build that out of any
    smaller decoders, as 'Decoder's cannot be combined (they are only 'Functor's).
    However, we can use a 'RecordDecoder' to build a 'Decoder' for @Project@:

>>> :{
project :: Decoder Project
project =
  record
    ( Project <$> field "name" strictText
              <*> field "description" strictText
              <*> field "stars" natural
    )
:}
-}
newtype RecordDecoder a =
  RecordDecoder
    ( Data.Functor.Product.Product
        ( Control.Applicative.Const
            (Dhall.Map.Map Text (Expector (Expr Src Void)))
        )
        ( Data.Functor.Compose.Compose ((->) (Expr Src Void)) (Extractor Src Void)
        )
        a
    )
  deriving (Functor, Applicative)

-- | Run a 'RecordDecoder' to build a 'Decoder'.
record :: RecordDecoder a -> Dhall.Marshal.Decode.Decoder a
record
    (RecordDecoder
        (Data.Functor.Product.Pair
            (Control.Applicative.Const fields)
            (Data.Functor.Compose.Compose extract)
        )
    ) = Decoder {..}
  where
    expected = Record <$> traverse (fmap Core.makeRecordField) fields

-- | Parse a single field of a record.
field :: Text -> Decoder a -> RecordDecoder a
field key (Decoder {..}) =
  RecordDecoder
    ( Data.Functor.Product.Pair
        ( Control.Applicative.Const
            (Dhall.Map.singleton key expected)
        )
        ( Data.Functor.Compose.Compose extractBody )
    )
  where
    extractBody expr@(RecordLit fields) = case Core.recordFieldValue <$> Dhall.Map.lookup key fields of
      Just v -> extract v
      _      -> typeError expected expr
    extractBody expr = typeError expected expr



{-| The 'UnionDecoder' monoid allows you to build a 'Decoder' from a Dhall union.

    For example, let's take the following Haskell data type:

>>> :{
data Status = Queued Natural
            | Result Text
            | Errored Text
:}

    And assume that we have the following Dhall union that we would like to
    parse as a @Status@:

> < Result : Text
> | Queued : Natural
> | Errored : Text
> >.Result "Finish successfully"

    Our decoder has type 'Decoder' @Status@, but we can't build that out of any
    smaller decoders, as 'Decoder's cannot be combined (they are only 'Functor's).
    However, we can use a 'UnionDecoder' to build a 'Decoder' for @Status@:

>>> :{
status :: Decoder Status
status = union
  (  ( Queued  <$> constructor "Queued"  natural )
  <> ( Result  <$> constructor "Result"  strictText )
  <> ( Errored <$> constructor "Errored" strictText )
  )
:}

-}
newtype UnionDecoder a =
    UnionDecoder
      ( Data.Functor.Compose.Compose (Dhall.Map.Map Text) Decoder a )
  deriving (Functor)

instance Semigroup (UnionDecoder a) where
    (<>) = coerce ((<>) :: Dhall.Map.Map Text (Decoder a) -> Dhall.Map.Map Text (Decoder a) -> Dhall.Map.Map Text (Decoder a))

instance Monoid (UnionDecoder a) where
    mempty = coerce (mempty :: Dhall.Map.Map Text (Decoder a))

-- | Run a 'UnionDecoder' to build a 'Decoder'.
union :: UnionDecoder a -> Decoder a
union (UnionDecoder (Data.Functor.Compose.Compose mp)) = Decoder {..}
  where
    extract expr = case expected' of
        Failure e -> Failure $ fmap ExpectedTypeError e
        Success x -> extract' expr x

    extract' e0 mp' = Data.Maybe.maybe (typeError expected e0) (uncurry Dhall.Marshal.Decode.extract) $ do
        (fld, e1, rest) <- extractUnionConstructor e0

        t <- Dhall.Map.lookup fld mp

        guard $
            Core.Union rest `Core.judgmentallyEqual` Core.Union (Dhall.Map.delete fld mp')

        pure (t, e1)

    expected = Union <$> expected'

    expected' = traverse (fmap notEmptyRecord . Dhall.Marshal.Decode.expected) mp

-- | Parse a single constructor of a union.
constructor :: Text -> Decoder a -> UnionDecoder a
constructor key valueDecoder = UnionDecoder
    ( Data.Functor.Compose.Compose (Dhall.Map.singleton key valueDecoder) )



{-| A newtype suitable for collecting one or more errors.
-}
newtype DhallErrors e = DhallErrors
   { getErrors :: NonEmpty e
   } deriving (Eq, Functor, Semigroup)

instance (Show (DhallErrors e), Typeable e) => Exception (DhallErrors e)

{-| Render a given prefix and some errors to a string.
-}
showDhallErrors :: Show e => String -> DhallErrors e -> String
showDhallErrors _   (DhallErrors (e :| [])) = show e
showDhallErrors ctx (DhallErrors es) = prefix <> (unlines . Data.List.NonEmpty.toList . fmap show $ es)
  where
    prefix =
        "Multiple errors were encountered" ++ ctx ++ ": \n\
        \                                               \n"

{-| One or more errors returned from extracting a Dhall expression to a
    Haskell expression.
-}
type ExtractErrors s a = DhallErrors (ExtractError s a)

instance (Pretty s, Pretty a, Typeable s, Typeable a) => Show (ExtractErrors s a) where
    show = showDhallErrors " during extraction"

{-| Extraction of a value can fail for two reasons, either a type mismatch (which should not happen,
    as expressions are type-checked against the expected type before being passed to @extract@), or
    a term-level error, described with a freeform text value.
-}
data ExtractError s a =
    TypeMismatch (InvalidDecoder s a)
  | ExpectedTypeError ExpectedTypeError
  | ExtractError Text

instance (Pretty s, Pretty a, Typeable s, Typeable a) => Show (ExtractError s a) where
  show (TypeMismatch e)      = show e
  show (ExpectedTypeError e) = show e
  show (ExtractError es)     =
      _ERROR <> ": Failed extraction                                                   \n\
      \                                                                                \n\
      \The expression type-checked successfully but the transformation to the target   \n\
      \type failed with the following error:                                           \n\
      \                                                                                \n\
      \" <> Data.Text.unpack es <> "\n\
      \                                                                                \n"

instance (Pretty s, Pretty a, Typeable s, Typeable a) => Exception (ExtractError s a)

{-| Useful synonym for the `Validation` type used when marshalling Dhall
    expressions.
-}
type Extractor s a = Validation (ExtractErrors s a)

{-| Generate a type error during extraction by specifying the expected type
    and the actual type.
    The expected type is not yet determined.
-}
typeError :: Expector (Expr s a) -> Expr s a -> Extractor s a b
typeError expected actual = Failure $ case expected of
    Failure e         -> fmap ExpectedTypeError e
    Success expected' -> DhallErrors $ pure $ TypeMismatch $ InvalidDecoder expected' actual

-- | Turn a `Data.Text.Text` message into an extraction failure.
extractError :: Text -> Extractor s a b
extractError = Failure . DhallErrors . pure . ExtractError

{-| Useful synonym for the equivalent `Either` type used when marshalling Dhall
    code.
-}
type MonadicExtractor s a = Either (ExtractErrors s a)

-- | Switches from an @Applicative@ extraction result, able to accumulate errors,
-- to a @Monad@ extraction result, able to chain sequential operations.
toMonadic :: Extractor s a b -> MonadicExtractor s a b
toMonadic = validationToEither

-- | Switches from a @Monad@ extraction result, able to chain sequential errors,
-- to an @Applicative@ extraction result, able to accumulate errors.
fromMonadic :: MonadicExtractor s a b -> Extractor s a b
fromMonadic = eitherToValidation

{-| Every `Decoder` must obey the contract that if an expression's type matches
    the `expected` type then the `extract` function must not fail with a type
    error.  However, decoding may still fail for other reasons (such as the
    decoder for `Data.Map.Set`s rejecting a Dhall @List@ with duplicate
    elements).

    This error type is used to indicate an internal error in the implementation
    of a `Decoder` where the expected type matched the Dhall expression, but the
    expression supplied to the extraction function did not match the expected
    type.  If this happens that means that the `Decoder` itself needs to be
    fixed.
-}
data InvalidDecoder s a = InvalidDecoder
  { invalidDecoderExpected   :: Expr s a
  , invalidDecoderExpression :: Expr s a
  }
  deriving (Typeable)

instance (Pretty s, Typeable s, Pretty a, Typeable a) => Exception (InvalidDecoder s a)

instance (Pretty s, Pretty a, Typeable s, Typeable a) => Show (InvalidDecoder s a) where
    show InvalidDecoder { .. } =
        _ERROR <> ": Invalid Dhall.Decoder                                               \n\
        \                                                                                \n\
        \Every Decoder must provide an extract function that does not fail with a type   \n\
        \error if an expression matches the expected type.  You provided a Decoder that  \n\
        \disobeys this contract                                                          \n\
        \                                                                                \n\
        \The Decoder provided has the expected dhall type:                               \n\
        \                                                                                \n\
        \" <> show txt0 <> "\n\
        \                                                                                \n\
        \and it threw a type error during extraction from the well-typed expression:     \n\
        \                                                                                \n\
        \" <> show txt1 <> "\n\
        \                                                                                \n"
        where
          txt0 = Dhall.Util.insert invalidDecoderExpected
          txt1 = Dhall.Util.insert invalidDecoderExpression

{-| Useful synonym for the `Validation` type used when marshalling Dhall
    expressions.
-}
type Expector = Validation ExpectedTypeErrors

{-| One or more errors returned when determining the Dhall type of a
    Haskell expression.
-}
type ExpectedTypeErrors = DhallErrors ExpectedTypeError

{-| Error type used when determining the Dhall type of a Haskell expression.
-}
data ExpectedTypeError = RecursiveTypeError
    deriving (Eq, Show)

instance Exception ExpectedTypeError

instance Show ExpectedTypeErrors where
    show = showDhallErrors " while determining the expected type"

_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"
