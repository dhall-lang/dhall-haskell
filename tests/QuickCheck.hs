{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheck where

import Codec.Serialise (DeserialiseFailure(..))
import Control.Monad (guard)
import Data.Hashable (Hashable)
import Dhall.Map (Map)
import Dhall.Core
    ( Chunks(..)
    , Const(..)
    , Directory(..)
    , Expr(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , Scheme(..)
    , URL(..)
    , Var(..)
    )
import Numeric.Natural (Natural)
import Test.QuickCheck
    (Arbitrary(..), Gen, Property, genericShrink, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)

import qualified Codec.Serialise
import qualified Data.Coerce
import qualified Dhall.Map
import qualified Data.Sequence
import qualified Dhall.Binary
import qualified Dhall.Core
import qualified Test.QuickCheck
import qualified Test.Tasty.QuickCheck

newtype DeserialiseFailureWithEq = D DeserialiseFailure
    deriving (Show)

instance Eq DeserialiseFailureWithEq where
    D (DeserialiseFailure aL bL) == D (DeserialiseFailure aR bR) =
        aL == aR && bL == bR

lift0 :: a -> Gen a
lift0 = pure

lift1 :: Arbitrary a => (a -> b) -> Gen b
lift1 f = f <$> arbitrary

lift2 :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> Gen c
lift2 f = f <$> arbitrary <*> arbitrary

lift3 :: (Arbitrary a, Arbitrary b, Arbitrary c) => (a -> b -> c -> d) -> Gen d
lift3 f = f <$> arbitrary <*> arbitrary <*> arbitrary

lift4
    :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => (a -> b -> c -> d -> e) -> Gen e
lift4 f = f <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

lift5
    :: ( Arbitrary a
       , Arbitrary b
       , Arbitrary c
       , Arbitrary d
       , Arbitrary e
       )
    => (a -> b -> c -> d -> e -> f) -> Gen f
lift5 f =
      f <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

lift6
    :: ( Arbitrary a
       , Arbitrary b
       , Arbitrary c
       , Arbitrary d
       , Arbitrary e
       , Arbitrary f
       )
    => (a -> b -> c -> d -> e -> f -> g) -> Gen g
lift6 f =
      f <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

natural :: (Arbitrary a, Num a) => Gen a
natural =
    Test.QuickCheck.frequency
        [ (7, arbitrary)
        , (1, fmap (\x -> x + (2 ^ (64 :: Int))) arbitrary)
        ]

integer :: (Arbitrary a, Num a) => Gen a
integer =
    Test.QuickCheck.frequency
        [ (7, arbitrary)
        , (1, fmap (\x -> x + (2 ^ (64 :: Int))) arbitrary)
        , (1, fmap (\x -> x - (2 ^ (64 :: Int))) arbitrary)
        ]

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
    arbitrary = do
        n   <- Test.QuickCheck.choose (0, 2)
        kvs <- Test.QuickCheck.vectorOf n ((,) <$> arbitrary <*> arbitrary)
        return (Dhall.Map.fromList kvs)

    shrink =
            map Dhall.Map.fromList
        .   shrink
        .   Dhall.Map.toList

instance (Arbitrary s, Arbitrary a) => Arbitrary (Chunks s a) where
    arbitrary = do
        n <- Test.QuickCheck.choose (0, 2)
        Chunks <$> Test.QuickCheck.vectorOf n arbitrary <*> arbitrary

    shrink = genericShrink

instance Arbitrary Const where
    arbitrary = Test.QuickCheck.oneof [ pure Type, pure Kind ]

    shrink = genericShrink

instance Arbitrary Directory where
    arbitrary = lift1 Directory

    shrink = genericShrink

averageDepth :: Natural
averageDepth = 3

averageNumberOfSubExpressions :: Double
averageNumberOfSubExpressions = 1 - 1 / fromIntegral averageDepth

probabilityOfNullaryConstructor :: Double
probabilityOfNullaryConstructor = 1 / fromIntegral averageDepth

numberOfConstructors :: Natural
numberOfConstructors = 50

instance (Arbitrary s, Arbitrary a) => Arbitrary (Expr s a) where
    arbitrary =
        Test.QuickCheck.suchThat
            (Test.QuickCheck.frequency
                [ ( 7, lift1 Const)
                , ( 7, lift1 Var)
                , ( 1, Test.QuickCheck.oneof [ lift2 (Lam "_"), lift3 Lam ])
                , ( 1, Test.QuickCheck.oneof [ lift2 (Pi "_"), lift3 Pi ])
                , ( 1, lift2 App)
                , ( 1, Test.QuickCheck.oneof [ lift3 (Let "_"), lift4 Let ])
                , ( 1, lift2 Annot)
                , ( 7, lift0 Bool)
                , ( 7, lift1 BoolLit)
                , ( 1, lift2 BoolAnd)
                , ( 1, lift2 BoolOr)
                , ( 1, lift2 BoolEQ)
                , ( 1, lift2 BoolNE)
                , ( 1, lift3 BoolIf)
                , ( 7, lift0 Natural)
                , ( 7, fmap NaturalLit natural)
                , ( 7, lift0 NaturalFold)
                , ( 7, lift0 NaturalBuild)
                , ( 7, lift0 NaturalIsZero)
                , ( 7, lift0 NaturalEven)
                , ( 7, lift0 NaturalOdd)
                , ( 7, lift0 NaturalToInteger)
                , ( 7, lift0 NaturalShow)
                , ( 1, lift2 NaturalPlus)
                , ( 1, lift2 NaturalTimes)
                , ( 7, lift0 Integer)
                , ( 7, fmap IntegerLit integer)
                , ( 7, lift0 IntegerShow)
                , ( 7, lift0 Double)
                , ( 7, lift1 DoubleLit)
                , ( 7, lift0 DoubleShow)
                , ( 7, lift0 Text)
                , ( 1, lift1 TextLit)
                , ( 1, lift2 TextAppend)
                , ( 7, lift0 List)
                , let listLit = do
                          n  <- Test.QuickCheck.choose (0, 3)
                          xs <- Test.QuickCheck.vectorOf n arbitrary
                          let ys = Data.Sequence.fromList xs
                          ListLit <$> arbitrary <*> pure ys

                  in  ( 1, listLit)
                , ( 1, lift2 ListAppend)
                , ( 7, lift0 ListBuild)
                , ( 7, lift0 ListFold)
                , ( 7, lift0 ListLength)
                , ( 7, lift0 ListHead)
                , ( 7, lift0 ListLast)
                , ( 7, lift0 ListIndexed)
                , ( 7, lift0 ListReverse)
                , ( 7, lift0 Optional)
                , ( 1, lift2 OptionalLit)
                , ( 7, lift0 OptionalFold)
                , ( 7, lift0 OptionalBuild)
                , ( 1, lift1 Record)
                , ( 1, lift1 RecordLit)
                , ( 1, lift1 Union)
                , ( 1, lift3 UnionLit)
                , ( 1, lift2 Combine)
                , ( 1, lift2 CombineTypes)
                , ( 1, lift2 Prefer)
                , ( 1, lift3 Merge)
                , ( 1, lift1 Constructors)
                , ( 1, lift2 Field)
                , ( 1, lift2 Project)
                , ( 7, lift1 Embed)
                ]
            )
            standardizedExpression

    shrink expression = filter standardizedExpression (genericShrink expression)

standardizedExpression :: Expr s a -> Bool
standardizedExpression (ListLit  Nothing  xs) = not (Data.Sequence.null xs)
standardizedExpression (ListLit (Just _ ) xs) = Data.Sequence.null xs
standardizedExpression (Note _ _            ) = False
standardizedExpression  _                     = True

instance Arbitrary File where
    arbitrary = lift2 File

    shrink = genericShrink

instance Arbitrary FilePrefix where
    arbitrary = Test.QuickCheck.oneof [ pure Absolute, pure Here, pure Home ]

    shrink = genericShrink

instance Arbitrary ImportType where
    arbitrary =
        Test.QuickCheck.suchThat
            (Test.QuickCheck.oneof
                [ lift2 Local
                , lift5 (\a b c d e -> Remote (URL a b c d e Nothing))
                , lift1 Env
                , lift0 Missing
                ]
            )
            standardizedImportType

    shrink importType =
        filter standardizedImportType (genericShrink importType)

standardizedImportType :: ImportType -> Bool
standardizedImportType (Remote (URL _ _ _ _ _ (Just _))) = False
standardizedImportType  _                                = True

instance Arbitrary ImportHashed where
    arbitrary =
        Test.QuickCheck.suchThat
            (lift1 (ImportHashed Nothing))
            standardizedImportHashed

    shrink (ImportHashed { importType = oldImportType, .. }) = do
        newImportType <- shrink oldImportType
        let importHashed = ImportHashed { importType = newImportType, .. }
        guard (standardizedImportHashed importHashed)
        return importHashed

standardizedImportHashed :: ImportHashed -> Bool
standardizedImportHashed (ImportHashed (Just _) _) = False
standardizedImportHashed  _                        = True

-- The standard does not yet specify how to encode `as Text`, so don't test it
-- yet
instance Arbitrary ImportMode where
    arbitrary = lift0 Code

    shrink = genericShrink

instance Arbitrary Import where
    arbitrary = lift2 Import

    shrink = genericShrink

instance Arbitrary Scheme where
    arbitrary = Test.QuickCheck.oneof [ pure HTTP, pure HTTPS ]

    shrink = genericShrink

instance Arbitrary URL where
    arbitrary = lift6 URL

    shrink = genericShrink

instance Arbitrary Var where
    arbitrary =
        Test.QuickCheck.oneof
            [ fmap (V "_") natural
            , lift1 (\t -> V t 0)
            , lift1 V <*> natural
            ]

    shrink = genericShrink

binaryRoundtrip :: Expr () Import -> Property
binaryRoundtrip expression =
        wrap
            (fmap
                Dhall.Binary.decode
                (Codec.Serialise.deserialiseOrFail
                  (Codec.Serialise.serialise
                    (Dhall.Binary.encode Dhall.Binary.defaultProtocolVersion expression)
                  )
                )
            )
    === wrap (Right (Right expression))
  where
    wrap
        :: Either DeserialiseFailure       a
        -> Either DeserialiseFailureWithEq a
    wrap = Data.Coerce.coerce

isNormalizedIsConsistentWithNormalize :: Expr () Import -> Property
isNormalizedIsConsistentWithNormalize expression =
        Dhall.Core.isNormalized expression
    === (Dhall.Core.normalize expression == expression)

quickcheckTests :: TestTree
quickcheckTests
    = Test.Tasty.QuickCheck.testProperties
        "QuickCheck"
        [ ( "Binary serialization should round-trip"
          , Test.QuickCheck.property binaryRoundtrip
          )
        , ( "isNormalized should be consistent with normalize"
          , Test.QuickCheck.property
              (Test.QuickCheck.withMaxSuccess 10000 isNormalizedIsConsistentWithNormalize)
          )
        ]
