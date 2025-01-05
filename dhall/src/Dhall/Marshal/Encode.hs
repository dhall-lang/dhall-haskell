{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-| Please read the "Dhall.Tutorial" module, which contains a tutorial explaining
    how to use the language, the compiler, and this library
-}

module Dhall.Marshal.Encode
    ( -- * General
      Encoder(..)
    , ToDhall(..)
    , Inject
    , inject

      -- * Building encoders

      -- ** Records
    , RecordEncoder(..)
    , recordEncoder
    , encodeField
    , encodeFieldWith
      -- ** Unions
    , UnionEncoder(..)
    , unionEncoder
    , encodeConstructor
    , encodeConstructorWith
    , (>|<)

      -- * Generic encoding
    , GenericToDhall(..)
    , genericToDhall
    , genericToDhallWith
    , genericToDhallWithInputNormalizer
    , InterpretOptions(..)
    , SingletonConstructors(..)
    , defaultInterpretOptions

      -- * Miscellaneous
    , InputNormalizer(..)
    , defaultInputNormalizer
    , Result
    , (>$<)
    , (>*<)

    -- * Re-exports
    , Natural
    , Seq
    , Text
    , Vector
    , Generic
    ) where

import Control.Monad.Trans.State.Strict
import Data.Functor.Contravariant           (Contravariant (..), Op (..), (>$<))
import Data.Functor.Contravariant.Divisible (Divisible (..), divided)
import Dhall.Parser                         (Src (..))
import Dhall.Syntax
    ( Chunks (..)
    , DhallDouble (..)
    , Expr (..)
    )
import GHC.Generics
import Prelude                              hiding (maybe, sequence)

import qualified Control.Applicative
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Functor.Product
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.HashSet
import qualified Data.Map
import qualified Data.Scientific
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Short
import qualified Data.Time            as Time
import qualified Data.Vector
import qualified Data.Void
import qualified Dhall.Core           as Core
import qualified Dhall.Map

import Dhall.Marshal.Internal

-- $setup
-- >>> :set -XRecordWildCards
-- >>> import Dhall.Pretty.Internal (prettyExpr)

{-| An @(Encoder a)@ represents a way to marshal a value of type @\'a\'@ from
    Haskell into Dhall.
-}
data Encoder a = Encoder
    { embed    :: a -> Expr Src Void
    -- ^ Embeds a Haskell value as a Dhall expression
    , declared :: Expr Src Void
    -- ^ Dhall type of the Haskell value
    }

instance Contravariant Encoder where
    contramap f (Encoder embed declared) = Encoder embed' declared
      where
        embed' x = embed (f x)

{-| This class is used by `Dhall.Marshal.Decode.FromDhall` instance for functions:

> instance (ToDhall a, FromDhall b) => FromDhall (a -> b)

    You can convert Dhall functions with "simple" inputs (i.e. instances of this
    class) into Haskell functions.  This works by:

    * Marshaling the input to the Haskell function into a Dhall expression (i.e.
      @x :: Expr Src Void@)
    * Applying the Dhall function (i.e. @f :: Expr Src Void@) to the Dhall input
      (i.e. @App f x@)
    * Normalizing the syntax tree (i.e. @normalize (App f x)@)
    * Marshaling the resulting Dhall expression back into a Haskell value

    This class auto-generates a default implementation for types that
    implement `Generic`.  This does not auto-generate an instance for recursive
    types.

    The default instance can be tweaked using 'genericToDhallWith'/'genericToDhallWithInputNormalizer'
    and custom 'InterpretOptions', or using
    [DerivingVia](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DerivingVia)
    and 'Dhall.Deriving.Codec' from "Dhall.Deriving".
-}
class ToDhall a where
    injectWith :: InputNormalizer -> Encoder a
    default injectWith
        :: (Generic a, GenericToDhall (Rep a)) => InputNormalizer -> Encoder a
    injectWith _ = genericToDhall

-- | A compatibility alias for `ToDhall`
type Inject = ToDhall
{-# DEPRECATED Inject "Use ToDhall instead" #-}

{-| Use the default input normalizer for injecting a value.

> inject = injectWith defaultInputNormalizer
-}
inject :: ToDhall a => Encoder a
inject = injectWith defaultInputNormalizer



instance ToDhall Void where
    injectWith _ = Encoder {..}
      where
        embed = Data.Void.absurd

        declared = Union mempty

instance ToDhall Bool where
    injectWith _ = Encoder {..}
      where
        embed = BoolLit

        declared = Bool

instance ToDhall Data.ByteString.Short.ShortByteString where
    injectWith options =
        contramap Data.ByteString.Short.fromShort (injectWith options)

instance ToDhall Data.ByteString.Lazy.ByteString where
    injectWith options =
        contramap Data.ByteString.Lazy.toStrict (injectWith options)

instance ToDhall Data.ByteString.ByteString where
    injectWith _ = Encoder {..}
      where
        embed bytes = BytesLit bytes

        declared = Bytes

instance ToDhall Data.Text.Short.ShortText where
    injectWith _ = Encoder {..}
      where
        embed text =
            TextLit (Chunks [] (Data.Text.Short.toText text))

        declared = Text

instance ToDhall Data.Text.Lazy.Text where
    injectWith _ = Encoder {..}
      where
        embed text =
            TextLit (Chunks [] (Data.Text.Lazy.toStrict text))

        declared = Text

instance ToDhall Text where
    injectWith _ = Encoder {..}
      where
        embed text = TextLit (Chunks [] text)

        declared = Text

instance {-# OVERLAPS #-} ToDhall String where
    injectWith inputNormalizer =
        contramap Data.Text.pack (injectWith inputNormalizer :: Encoder Text)

instance ToDhall Natural where
    injectWith _ = Encoder {..}
      where
        embed = NaturalLit

        declared = Natural

instance ToDhall Integer where
    injectWith _ = Encoder {..}
      where
        embed = IntegerLit

        declared = Integer

instance ToDhall Int where
    injectWith _ = Encoder {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

instance ToDhall Int8 where
    injectWith _ = Encoder {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

instance ToDhall Int16 where
    injectWith _ = Encoder {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

instance ToDhall Int32 where
    injectWith _ = Encoder {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

instance ToDhall Int64 where
    injectWith _ = Encoder {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

{-| Encode a 'Word' to a Dhall @Natural@.

>>> embed inject (12 :: Word)
NaturalLit 12
-}
instance ToDhall Word where
    injectWith _ = Encoder {..}
      where
        embed = NaturalLit . fromIntegral

        declared = Natural

{-| Encode a 'Word8' to a Dhall @Natural@.

>>> embed inject (12 :: Word8)
NaturalLit 12
-}
instance ToDhall Word8 where
    injectWith _ = Encoder {..}
      where
        embed = NaturalLit . fromIntegral

        declared = Natural

{-| Encode a 'Word16' to a Dhall @Natural@.

>>> embed inject (12 :: Word16)
NaturalLit 12
-}
instance ToDhall Word16 where
    injectWith _ = Encoder {..}
      where
        embed = NaturalLit . fromIntegral

        declared = Natural

{-| Encode a 'Word32' to a Dhall @Natural@.

>>> embed inject (12 :: Word32)
NaturalLit 12
-}
instance ToDhall Word32 where
    injectWith _ = Encoder {..}
      where
        embed = NaturalLit . fromIntegral

        declared = Natural

{-| Encode a 'Word64' to a Dhall @Natural@.

>>> embed inject (12 :: Word64)
NaturalLit 12
-}
instance ToDhall Word64 where
    injectWith _ = Encoder {..}
      where
        embed = NaturalLit . fromIntegral

        declared = Natural

instance ToDhall Double where
    injectWith _ = Encoder {..}
      where
        embed = DoubleLit . DhallDouble

        declared = Double

instance ToDhall Scientific where
    injectWith inputNormalizer =
        contramap Data.Scientific.toRealFloat (injectWith inputNormalizer :: Encoder Double)

instance ToDhall () where
    injectWith _ = Encoder {..}
      where
        embed = const (RecordLit mempty)

        declared = Record mempty

instance ToDhall a => ToDhall (Maybe a) where
    injectWith inputNormalizer = Encoder embedOut declaredOut
      where
        embedOut (Just x ) = Some (embedIn x)
        embedOut  Nothing  = App None declaredIn

        Encoder embedIn declaredIn = injectWith inputNormalizer

        declaredOut = App Optional declaredIn

instance ToDhall a => ToDhall (Seq a) where
    injectWith inputNormalizer = Encoder embedOut declaredOut
      where
        embedOut xs = ListLit listType (fmap embedIn xs)
          where
            listType
                | null xs   = Just (App List declaredIn)
                | otherwise = Nothing

        declaredOut = App List declaredIn

        Encoder embedIn declaredIn = injectWith inputNormalizer

instance ToDhall a => ToDhall [a] where
    injectWith = fmap (contramap Data.Sequence.fromList) injectWith

instance ToDhall a => ToDhall (Vector a) where
    injectWith = fmap (contramap Data.Vector.toList) injectWith

instance ToDhall Time.TimeOfDay where
    injectWith _ = Encoder {..}
      where
        embed timeOfDay = TimeLiteral timeOfDay 12

        declared = Time

instance ToDhall Time.Day where
    injectWith _ = Encoder {..}
      where
        embed = DateLiteral

        declared = Date

instance ToDhall Time.TimeZone where
    injectWith _ = Encoder {..}
      where
        embed = TimeZoneLiteral

        declared = TimeZone

instance ToDhall Time.LocalTime where
    injectWith _ = recordEncoder $
      adapt
        >$< encodeField "date"
        >*< encodeField "time"
      where
        adapt (Time.LocalTime date time) = (date, time)

instance ToDhall Time.ZonedTime where
    injectWith _ = recordEncoder $
      adapt
        >$< encodeField "date"
        >*< encodeField "time"
        >*< encodeField "timeZone"
      where
        adapt (Time.ZonedTime (Time.LocalTime date time) timeZone) = (date, (time, timeZone))

instance ToDhall Time.UTCTime where
    injectWith = contramap (Time.utcToZonedTime Time.utc) . injectWith

instance ToDhall Time.DayOfWeek where
    injectWith _ = Encoder{..}
      where
        embed Time.Sunday =
            Field declared (Core.makeFieldSelection "Sunday")
        embed Time.Monday =
            Field declared (Core.makeFieldSelection "Monday" )
        embed Time.Tuesday =
            Field declared (Core.makeFieldSelection "Tuesday")
        embed Time.Wednesday =
            Field declared (Core.makeFieldSelection "Wednesday")
        embed Time.Thursday =
            Field declared (Core.makeFieldSelection "Thursday")
        embed Time.Friday =
            Field declared (Core.makeFieldSelection "Friday")
        embed Time.Saturday =
            Field declared (Core.makeFieldSelection "Saturday")

        declared =
            Union
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

{-| Note that the output list will be sorted.

>>> let x = Data.Set.fromList ["mom", "hi" :: Text]
>>> prettyExpr $ embed inject x
[ "hi", "mom" ]

-}
instance ToDhall a => ToDhall (Data.Set.Set a) where
    injectWith = fmap (contramap Data.Set.toAscList) injectWith

-- | Note that the output list may not be sorted
instance ToDhall a => ToDhall (Data.HashSet.HashSet a) where
    injectWith = fmap (contramap Data.HashSet.toList) injectWith

instance (ToDhall a, ToDhall b) => ToDhall (a, b)

{-| Embed a `Data.Map` as a @Prelude.Map.Type@.

>>> prettyExpr $ embed inject (Data.Map.fromList [(1 :: Natural, True)])
[ { mapKey = 1, mapValue = True } ]

>>> prettyExpr $ embed inject (Data.Map.fromList [] :: Data.Map.Map Natural Bool)
[] : List { mapKey : Natural, mapValue : Bool }

-}
instance (ToDhall k, ToDhall v) => ToDhall (Data.Map.Map k v) where
    injectWith inputNormalizer = Encoder embedOut declaredOut
      where
        embedOut m = ListLit listType (mapEntries m)
          where
            listType
                | Data.Map.null m = Just declaredOut
                | otherwise       = Nothing

        declaredOut = App List (Record $ Dhall.Map.fromList
                          [ ("mapKey", Core.makeRecordField declaredK)
                          , ("mapValue", Core.makeRecordField declaredV)
                          ])

        mapEntries = Data.Sequence.fromList . fmap recordPair . Data.Map.toList
        recordPair (k, v) = RecordLit $ Dhall.Map.fromList
                                [ ("mapKey", Core.makeRecordField $ embedK k)
                                , ("mapValue", Core.makeRecordField $ embedV v)
                                ]

        Encoder embedK declaredK = injectWith inputNormalizer
        Encoder embedV declaredV = injectWith inputNormalizer

{-| Embed a `Data.HashMap` as a @Prelude.Map.Type@.

>>> prettyExpr $ embed inject (HashMap.fromList [(1 :: Natural, True)])
[ { mapKey = 1, mapValue = True } ]

>>> prettyExpr $ embed inject (HashMap.fromList [] :: HashMap Natural Bool)
[] : List { mapKey : Natural, mapValue : Bool }

-}
instance (ToDhall k, ToDhall v) => ToDhall (HashMap k v) where
    injectWith inputNormalizer = Encoder embedOut declaredOut
      where
        embedOut m = ListLit listType (mapEntries m)
          where
            listType
                | HashMap.null m = Just declaredOut
                | otherwise       = Nothing

        declaredOut = App List (Record $ Dhall.Map.fromList
                          [ ("mapKey", Core.makeRecordField declaredK)
                          , ("mapValue", Core.makeRecordField declaredV)
                          ])

        mapEntries = Data.Sequence.fromList . fmap recordPair . HashMap.toList
        recordPair (k, v) = RecordLit $ Dhall.Map.fromList
                                [ ("mapKey", Core.makeRecordField $ embedK k)
                                , ("mapValue", Core.makeRecordField $ embedV v)
                                ]

        Encoder embedK declaredK = injectWith inputNormalizer
        Encoder embedV declaredV = injectWith inputNormalizer

instance ToDhall (f (Result f)) => ToDhall (Result f) where
    injectWith inputNormalizer = Encoder {..}
      where
        embed = App "Make" . Dhall.Marshal.Encode.embed (injectWith inputNormalizer) . _unResult
        declared = "result"

instance forall f. (Functor f, ToDhall (f (Result f))) => ToDhall (Fix f) where
    injectWith inputNormalizer = Encoder {..}
      where
        embed fixf =
          Lam Nothing (Core.makeFunctionBinding "result" (Const Core.Type)) $
            Lam Nothing (Core.makeFunctionBinding "Make" makeType) $
              embed' . fixToResult $ fixf

        declared = Pi Nothing "result" (Const Core.Type) $ Pi Nothing "_" makeType "result"

        makeType = Pi Nothing "_" declared' "result"
        Encoder embed' _ = injectWith @(Dhall.Marshal.Internal.Result f) inputNormalizer
        Encoder _ declared' = injectWith @(f (Dhall.Marshal.Internal.Result f)) inputNormalizer

fixToResult :: Functor f => Fix f -> Result f
fixToResult (Fix x) = Result (fmap fixToResult x)



{-| This is the underlying class that powers the `Dhall.Marshal.Decode.FromDhall` class's support
    for automatically deriving a generic implementation.
-}
class GenericToDhall f where
    genericToDhallWithNormalizer :: InputNormalizer -> InterpretOptions -> State Int (Encoder (f a))

instance GenericToDhall f => GenericToDhall (M1 D d f) where
    genericToDhallWithNormalizer inputNormalizer options = do
        res <- genericToDhallWithNormalizer inputNormalizer options
        pure (contramap unM1 res)

instance GenericToDhall f => GenericToDhall (M1 C c f) where
    genericToDhallWithNormalizer inputNormalizer options = do
        res <- genericToDhallWithNormalizer inputNormalizer options
        pure (contramap unM1 res)

instance (Selector s, ToDhall a) => GenericToDhall (M1 S s (K1 i a)) where
    genericToDhallWithNormalizer inputNormalizer InterpretOptions{..} = do
        let Encoder { embed = embed', declared = declared' } =
                injectWith inputNormalizer

        let n :: M1 S s (K1 i a) r
            n = undefined

        name <- fieldModifier <$> getSelName n

        let embed0 (M1 (K1 x)) = embed' x

        let embed1 (M1 (K1 x)) =
                RecordLit (Dhall.Map.singleton name (Core.makeRecordField $ embed' x))

        let embed =
                case singletonConstructors of
                    Bare                    -> embed0
                    Smart | selName n == "" -> embed0
                    _                       -> embed1

        let declared =
                case singletonConstructors of
                    Bare ->
                        declared'
                    Smart | selName n == "" ->
                        declared'
                    _ ->
                        Record (Dhall.Map.singleton name $ Core.makeRecordField declared')

        return (Encoder {..})

instance (Constructor c1, Constructor c2, GenericToDhall f1, GenericToDhall f2) => GenericToDhall (M1 C c1 f1 :+: M1 C c2 f2) where
    genericToDhallWithNormalizer inputNormalizer options@(InterpretOptions {..}) = pure (Encoder {..})
      where
        embed (L1 (M1 l)) =
            case notEmptyRecordLit (embedL l) of
                Nothing ->
                    Field declared $ Core.makeFieldSelection keyL
                Just valL ->
                    App (Field declared $ Core.makeFieldSelection keyL) valL

        embed (R1 (M1 r)) =
            case notEmptyRecordLit (embedR r) of
                Nothing ->
                    Field declared $ Core.makeFieldSelection keyR
                Just valR ->
                    App (Field declared $ Core.makeFieldSelection keyR) valR

        declared =
            Union
                (Dhall.Map.fromList
                    [ (keyL, notEmptyRecord declaredL)
                    , (keyR, notEmptyRecord declaredR)
                    ]
                )

        nL :: M1 i c1 f1 a
        nL = undefined

        nR :: M1 i c2 f2 a
        nR = undefined

        keyL = constructorModifier (Data.Text.pack (conName nL))
        keyR = constructorModifier (Data.Text.pack (conName nR))

        Encoder embedL declaredL = evalState (genericToDhallWithNormalizer inputNormalizer options) 1
        Encoder embedR declaredR = evalState (genericToDhallWithNormalizer inputNormalizer options) 1

instance (Constructor c, GenericToDhall (f :+: g), GenericToDhall h) => GenericToDhall ((f :+: g) :+: M1 C c h) where
    genericToDhallWithNormalizer inputNormalizer options@(InterpretOptions {..}) = pure (Encoder {..})
      where
        embed (L1 l) =
            case maybeValL of
                Nothing   -> Field declared $ Core.makeFieldSelection keyL
                Just valL -> App (Field declared $ Core.makeFieldSelection keyL) valL
          where
            (keyL, maybeValL) =
              unsafeExpectUnionLit "genericToDhallWithNormalizer (:+:)" (embedL l)
        embed (R1 (M1 r)) =
            case notEmptyRecordLit (embedR r) of
                Nothing   -> Field declared $ Core.makeFieldSelection keyR
                Just valR -> App (Field declared $ Core.makeFieldSelection keyR) valR

        nR :: M1 i c h a
        nR = undefined

        keyR = constructorModifier (Data.Text.pack (conName nR))

        declared = Union (Dhall.Map.insert keyR (notEmptyRecord declaredR) ktsL)

        Encoder embedL declaredL = evalState (genericToDhallWithNormalizer inputNormalizer options) 1
        Encoder embedR declaredR = evalState (genericToDhallWithNormalizer inputNormalizer options) 1

        ktsL = unsafeExpectUnion "genericToDhallWithNormalizer (:+:)" declaredL

instance (Constructor c, GenericToDhall f, GenericToDhall (g :+: h)) => GenericToDhall (M1 C c f :+: (g :+: h)) where
    genericToDhallWithNormalizer inputNormalizer options@(InterpretOptions {..}) = pure (Encoder {..})
      where
        embed (L1 (M1 l)) =
            case notEmptyRecordLit (embedL l) of
                Nothing   -> Field declared $ Core.makeFieldSelection keyL
                Just valL -> App (Field declared $ Core.makeFieldSelection keyL) valL
        embed (R1 r) =
            case maybeValR of
                Nothing   -> Field declared $ Core.makeFieldSelection keyR
                Just valR -> App (Field declared $ Core.makeFieldSelection keyR) valR
          where
            (keyR, maybeValR) =
                unsafeExpectUnionLit "genericToDhallWithNormalizer (:+:)" (embedR r)

        nL :: M1 i c f a
        nL = undefined

        keyL = constructorModifier (Data.Text.pack (conName nL))

        declared = Union (Dhall.Map.insert keyL (notEmptyRecord declaredL) ktsR)

        Encoder embedL declaredL = evalState (genericToDhallWithNormalizer inputNormalizer options) 1
        Encoder embedR declaredR = evalState (genericToDhallWithNormalizer inputNormalizer options) 1

        ktsR = unsafeExpectUnion "genericToDhallWithNormalizer (:+:)" declaredR

instance (GenericToDhall (f :+: g), GenericToDhall (h :+: i)) => GenericToDhall ((f :+: g) :+: (h :+: i)) where
    genericToDhallWithNormalizer inputNormalizer options = pure (Encoder {..})
      where
        embed (L1 l) =
            case maybeValL of
                Nothing   -> Field declared $ Core.makeFieldSelection keyL
                Just valL -> App (Field declared $ Core.makeFieldSelection keyL) valL
          where
            (keyL, maybeValL) =
                unsafeExpectUnionLit "genericToDhallWithNormalizer (:+:)" (embedL l)
        embed (R1 r) =
            case maybeValR of
                Nothing   -> Field declared $ Core.makeFieldSelection keyR
                Just valR -> App (Field declared $ Core.makeFieldSelection keyR) valR
          where
            (keyR, maybeValR) =
                unsafeExpectUnionLit "genericToDhallWithNormalizer (:+:)" (embedR r)

        declared = Union (Dhall.Map.union ktsL ktsR)

        Encoder embedL declaredL = evalState (genericToDhallWithNormalizer inputNormalizer options) 1
        Encoder embedR declaredR = evalState (genericToDhallWithNormalizer inputNormalizer options) 1

        ktsL = unsafeExpectUnion "genericToDhallWithNormalizer (:+:)" declaredL
        ktsR = unsafeExpectUnion "genericToDhallWithNormalizer (:+:)" declaredR

instance (GenericToDhall (f :*: g), GenericToDhall (h :*: i)) => GenericToDhall ((f :*: g) :*: (h :*: i)) where
    genericToDhallWithNormalizer inputNormalizer options = do
        Encoder embedL declaredL <- genericToDhallWithNormalizer inputNormalizer options
        Encoder embedR declaredR <- genericToDhallWithNormalizer inputNormalizer options

        let embed (l :*: r) =
                RecordLit (Dhall.Map.union mapL mapR)
              where
                mapL =
                    unsafeExpectRecordLit "genericToDhallWithNormalizer (:*:)" (embedL l)

                mapR =
                    unsafeExpectRecordLit "genericToDhallWithNormalizer (:*:)" (embedR r)

        let declared = Record (Dhall.Map.union mapL mapR)
              where
                mapL = unsafeExpectRecord "genericToDhallWithNormalizer (:*:)" declaredL
                mapR = unsafeExpectRecord "genericToDhallWithNormalizer (:*:)" declaredR

        pure (Encoder {..})

instance (GenericToDhall (f :*: g), Selector s, ToDhall a) => GenericToDhall ((f :*: g) :*: M1 S s (K1 i a)) where
    genericToDhallWithNormalizer inputNormalizer options@InterpretOptions{..} = do
        let nR :: M1 S s (K1 i a) r
            nR = undefined

        nameR <- fmap fieldModifier (getSelName nR)

        Encoder embedL declaredL <- genericToDhallWithNormalizer inputNormalizer options

        let Encoder embedR declaredR = injectWith inputNormalizer

        let embed (l :*: M1 (K1 r)) =
                RecordLit (Dhall.Map.insert nameR (Core.makeRecordField $ embedR r) mapL)
              where
                mapL =
                    unsafeExpectRecordLit "genericToDhallWithNormalizer (:*:)" (embedL l)

        let declared = Record (Dhall.Map.insert nameR (Core.makeRecordField declaredR) mapL)
              where
                mapL = unsafeExpectRecord "genericToDhallWithNormalizer (:*:)" declaredL

        return (Encoder {..})

instance (Selector s, ToDhall a, GenericToDhall (f :*: g)) => GenericToDhall (M1 S s (K1 i a) :*: (f :*: g)) where
    genericToDhallWithNormalizer inputNormalizer options@InterpretOptions{..} = do
        let nL :: M1 S s (K1 i a) r
            nL = undefined

        nameL <- fmap fieldModifier (getSelName nL)

        let Encoder embedL declaredL = injectWith inputNormalizer

        Encoder embedR declaredR <- genericToDhallWithNormalizer inputNormalizer options

        let embed (M1 (K1 l) :*: r) =
                RecordLit (Dhall.Map.insert nameL (Core.makeRecordField $ embedL l) mapR)
              where
                mapR =
                    unsafeExpectRecordLit "genericToDhallWithNormalizer (:*:)" (embedR r)

        let declared = Record (Dhall.Map.insert nameL (Core.makeRecordField declaredL) mapR)
              where
                mapR = unsafeExpectRecord "genericToDhallWithNormalizer (:*:)" declaredR

        return (Encoder {..})

instance (Selector s1, Selector s2, ToDhall a1, ToDhall a2) => GenericToDhall (M1 S s1 (K1 i1 a1) :*: M1 S s2 (K1 i2 a2)) where
    genericToDhallWithNormalizer inputNormalizer InterpretOptions{..} = do
        let nL :: M1 S s1 (K1 i1 a1) r
            nL = undefined

        let nR :: M1 S s2 (K1 i2 a2) r
            nR = undefined

        nameL <- fmap fieldModifier (getSelName nL)
        nameR <- fmap fieldModifier (getSelName nR)

        let Encoder embedL declaredL = injectWith inputNormalizer
        let Encoder embedR declaredR = injectWith inputNormalizer

        let embed (M1 (K1 l) :*: M1 (K1 r)) =
                RecordLit $
                    Dhall.Map.fromList
                        [ (nameL, Core.makeRecordField $ embedL l)
                        , (nameR, Core.makeRecordField $ embedR r) ]


        let declared =
                Record $ Dhall.Map.fromList
                    [ (nameL, Core.makeRecordField declaredL)
                    , (nameR, Core.makeRecordField declaredR) ]


        return (Encoder {..})

instance GenericToDhall U1 where
    genericToDhallWithNormalizer _ _ = pure (Encoder {..})
      where
        embed _ = RecordLit mempty

        declared = Record mempty

{-| Use the default options for injecting a value, whose structure is
determined generically.

This can be used when you want to use 'ToDhall' on types that you don't
want to define orphan instances for.
-}
genericToDhall
  :: (Generic a, GenericToDhall (Rep a)) => Encoder a
genericToDhall
    = genericToDhallWith defaultInterpretOptions

{-| Use custom options for injecting a value, whose structure is
determined generically.

This can be used when you want to use 'ToDhall' on types that you don't
want to define orphan instances for.
-}
genericToDhallWith
  :: (Generic a, GenericToDhall (Rep a)) => InterpretOptions -> Encoder a
genericToDhallWith options = genericToDhallWithInputNormalizer options defaultInputNormalizer

{-| `genericToDhallWithInputNormalizer` is like `genericToDhallWith`, but
    instead of using the `defaultInputNormalizer` it expects an custom
    `InputNormalizer`.
-}
genericToDhallWithInputNormalizer
  :: (Generic a, GenericToDhall (Rep a)) => InterpretOptions -> InputNormalizer -> Encoder a
genericToDhallWithInputNormalizer options inputNormalizer
    = contramap GHC.Generics.from (evalState (genericToDhallWithNormalizer inputNormalizer options) 1)



{-| The 'RecordEncoder' divisible (contravariant) functor allows you to build
    an 'Encoder' for a Dhall record.

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

    Our encoder has type 'Encoder' @Project@, but we can't build that out of any
    smaller encoders, as 'Encoder's cannot be combined (they are only 'Contravariant's).
    However, we can use an 'RecordEncoder' to build an 'Encoder' for @Project@:

>>> :{
injectProject :: Encoder Project
injectProject =
  recordEncoder
    ( adapt >$< encodeFieldWith "name" inject
            >*< encodeFieldWith "description" inject
            >*< encodeFieldWith "stars" inject
    )
  where
    adapt (Project{..}) = (projectName, (projectDescription, projectStars))
:}

    Or, since we are simply using the `ToDhall` instance to inject each field, we could write

>>> :{
injectProject :: Encoder Project
injectProject =
  recordEncoder
    ( adapt >$< encodeField "name"
            >*< encodeField "description"
            >*< encodeField "stars"
    )
  where
    adapt (Project{..}) = (projectName, (projectDescription, projectStars))
:}

-}
newtype RecordEncoder a
  = RecordEncoder (Dhall.Map.Map Text (Encoder a))

instance Contravariant RecordEncoder where
  contramap f (RecordEncoder encodeTypeRecord) = RecordEncoder $ contramap f <$> encodeTypeRecord

instance Divisible RecordEncoder where
  divide f (RecordEncoder bEncoderRecord) (RecordEncoder cEncoderRecord) =
      RecordEncoder
    $ Dhall.Map.union
      ((contramap $ fst . f) <$> bEncoderRecord)
      ((contramap $ snd . f) <$> cEncoderRecord)
  conquer = RecordEncoder mempty

-- | Convert a `RecordEncoder` into the equivalent `Encoder`.
recordEncoder :: RecordEncoder a -> Encoder a
recordEncoder (RecordEncoder encodeTypeRecord) = Encoder makeRecordLit recordType
  where
    recordType = Record $ (Core.makeRecordField . declared) <$> encodeTypeRecord
    makeRecordLit x = RecordLit $ (Core.makeRecordField . ($ x) . embed) <$> encodeTypeRecord

{-| Specify how to encode one field of a record using the default `ToDhall`
    instance for that type.
-}
encodeField :: ToDhall a => Text -> RecordEncoder a
encodeField name = encodeFieldWith name inject

{-| Specify how to encode one field of a record by supplying an explicit
    `Encoder` for that field.
-}
encodeFieldWith :: Text -> Encoder a -> RecordEncoder a
encodeFieldWith name encodeType = RecordEncoder $ Dhall.Map.singleton name encodeType



{-| 'UnionEncoder' allows you to build an 'Encoder' for a Dhall record.

    For example, let's take the following Haskell data type:

>>> :{
data Status = Queued Natural
            | Result Text
            | Errored Text
            | Unreachable
:}

    And assume that we have the following Dhall union that we would like to
    parse as a @Status@:

> < Result : Text
> | Queued : Natural
> | Errored : Text
> | Unreachable
> >.Result "Finish successfully"

    Our encoder has type 'Encoder' @Status@, but we can't build that out of any
    smaller encoders, as 'Encoder's cannot be combined.
    However, we can use an 'UnionEncoder' to build an 'Encoder' for @Status@:

>>> :{
injectStatus :: Encoder Status
injectStatus = adapt >$< unionEncoder
  (   encodeConstructorWith "Queued"  inject
  >|< encodeConstructorWith "Result"  inject
  >|< encodeConstructorWith "Errored" inject
  >|< encodeConstructorWith "Unreachable" inject
  )
  where
    adapt (Queued  n) = Left n
    adapt (Result  t) = Right (Left t)
    adapt (Errored e) = Right (Right (Left e))
    adapt Unreachable = Right (Right (Right ()))
:}

    Or, since we are simply using the `ToDhall` instance to inject each branch, we could write

>>> :{
injectStatus :: Encoder Status
injectStatus = adapt >$< unionEncoder
  (   encodeConstructor "Queued"
  >|< encodeConstructor "Result"
  >|< encodeConstructor "Errored"
  >|< encodeConstructor "Unreachable"
  )
  where
    adapt (Queued  n) = Left n
    adapt (Result  t) = Right (Left t)
    adapt (Errored e) = Right (Right (Left e))
    adapt Unreachable = Right (Right (Right ()))
:}

-}
newtype UnionEncoder a =
  UnionEncoder
    ( Data.Functor.Product.Product
        ( Control.Applicative.Const
            ( Dhall.Map.Map
                Text
                ( Expr Src Void )
            )
        )
        ( Op (Text, Expr Src Void) )
        a
    )
  deriving (Contravariant)

-- | Convert a `UnionEncoder` into the equivalent `Encoder`.
unionEncoder :: UnionEncoder a -> Encoder a
unionEncoder ( UnionEncoder ( Data.Functor.Product.Pair ( Control.Applicative.Const fields ) ( Op embedF ) ) ) =
    Encoder
      { embed = \x ->
          let (name, y) = embedF x
          in  case notEmptyRecordLit y of
                  Nothing  -> Field (Union fields') $ Core.makeFieldSelection name
                  Just val -> App (Field (Union fields') $ Core.makeFieldSelection name) val
      , declared =
          Union fields'
      }
  where
    fields' = fmap notEmptyRecord fields

{-| Specify how to encode an alternative by using the default `ToDhall` instance
    for that type.
-}
encodeConstructor
    :: ToDhall a
    => Text
    -> UnionEncoder a
encodeConstructor name = encodeConstructorWith name inject

{-| Specify how to encode an alternative by providing an explicit `Encoder`
    for that alternative.
-}
encodeConstructorWith
    :: Text
    -> Encoder a
    -> UnionEncoder a
encodeConstructorWith name encodeType = UnionEncoder $
    Data.Functor.Product.Pair
      ( Control.Applicative.Const
          ( Dhall.Map.singleton
              name
              ( declared encodeType )
          )
      )
      ( Op ( (name,) . embed encodeType )
      )

-- | Combines two 'UnionEncoder' values.  See 'UnionEncoder' for usage
-- notes.
--
-- Ideally, this matches 'Data.Functor.Contravariant.Divisible.chosen';
-- however, this allows 'UnionEncoder' to not need a 'Divisible' instance
-- itself (since no instance is possible).
(>|<) :: UnionEncoder a -> UnionEncoder b -> UnionEncoder (Either a b)
UnionEncoder (Data.Functor.Product.Pair (Control.Applicative.Const mx) (Op fx))
    >|< UnionEncoder (Data.Functor.Product.Pair (Control.Applicative.Const my) (Op fy)) =
    UnionEncoder
      ( Data.Functor.Product.Pair
          ( Control.Applicative.Const (mx <> my) )
          ( Op (either fx fy) )
      )

infixr 5 >|<



-- | Infix 'divided'
(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) = divided

infixr 5 >*<
