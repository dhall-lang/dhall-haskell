{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Test.QuickCheck where

import Codec.Serialise (DeserialiseFailure(..))
import Data.Either (isRight)
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

import Data.Functor.Identity (Identity(..))
import Dhall.Set (Set)
import Dhall.Src (Src(..))
import Dhall.TypeCheck (Typer, TypeError)
import Numeric.Natural (Natural)
import Test.QuickCheck
    (Arbitrary(..), Gen, Positive(..), Property, NonNegative(..), genericShrink, (===), (==>))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (QuickCheckTests(..))
import Text.Megaparsec (SourcePos(..), Pos)

import qualified Control.Spoon
import qualified Codec.Serialise
import qualified Data.Coerce
import qualified Data.List
import qualified Data.Sequence
import qualified Dhall.Binary
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Diff
import qualified Dhall.Map
import qualified Dhall.Set
import qualified Dhall.TypeCheck
import qualified Test.QuickCheck
import qualified Test.Tasty
import qualified Test.Tasty.QuickCheck
import qualified Text.Megaparsec       as Megaparsec

newtype DeserialiseFailureWithEq = D DeserialiseFailure
    deriving (Show)

instance Eq DeserialiseFailureWithEq where
    D (DeserialiseFailure aL bL) == D (DeserialiseFailure aR bR) =
        aL == aR && bL == bR

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = Dhall.Set.fromList <$> arbitrary
  shrink = map Dhall.Set.fromList . shrink . Dhall.Set.toList

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

integer :: (Arbitrary a, Num a) => Gen a
integer =
    Test.QuickCheck.frequency
        [ (7, arbitrary)
        , (1, fmap (\x -> x + (2 ^ (64 :: Int))) arbitrary)
        , (1, fmap (\x -> x - (2 ^ (64 :: Int))) arbitrary)
        ]

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
    arbitrary = do
        n   <- Test.QuickCheck.choose (0, 2)
        kvs <- Test.QuickCheck.vectorOf n ((,) <$> arbitrary <*> arbitrary)
        -- Sorting the fields here because serialization needs them in order
        return (Dhall.Map.fromList (Data.List.sortOn fst kvs))

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
    arbitrary = Test.QuickCheck.oneof [ pure Type, pure Kind, pure Sort ]

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
                , ( 7, Test.QuickCheck.oneof [ lift3 (Let "_"), lift4 Let ])
                , ( 1, lift2 Annot)
                , ( 1, lift0 Bool)
                , ( 7, lift1 BoolLit)
                , ( 1, lift2 BoolAnd)
                , ( 1, lift2 BoolOr)
                , ( 1, lift2 BoolEQ)
                , ( 1, lift2 BoolNE)
                , ( 1, lift3 BoolIf)
                , ( 1, lift0 Natural)
                , ( 7, fmap NaturalLit arbitrary)
                , ( 1, lift0 NaturalFold)
                , ( 1, lift0 NaturalBuild)
                , ( 1, lift0 NaturalIsZero)
                , ( 1, lift0 NaturalEven)
                , ( 1, lift0 NaturalOdd)
                , ( 1, lift0 NaturalToInteger)
                , ( 1, lift0 NaturalShow)
                , ( 1, lift2 NaturalPlus)
                , ( 1, lift2 NaturalTimes)
                , ( 1, lift0 NaturalSubtract)
                , ( 1, lift0 Integer)
                , ( 7, fmap IntegerLit integer)
                , ( 1, lift0 IntegerShow)
                , ( 1, lift0 Double)
                , ( 7, lift1 DoubleLit)
                , ( 1, lift0 DoubleShow)
                , ( 1, lift0 Text)
                , ( 1, lift1 TextLit)
                , ( 1, lift2 TextAppend)
                , ( 1, lift0 List)
                , let listLit = do
                          n  <- Test.QuickCheck.choose (0, 3)
                          xs <- Test.QuickCheck.vectorOf n arbitrary
                          let ys = Data.Sequence.fromList xs
                          ListLit <$> arbitrary <*> pure ys

                  in  ( 1, listLit)
                , ( 1, lift2 ListAppend)
                , ( 1, lift0 ListBuild)
                , ( 1, lift0 ListFold)
                , ( 1, lift0 ListLength)
                , ( 1, lift0 ListHead)
                , ( 1, lift0 ListLast)
                , ( 1, lift0 ListIndexed)
                , ( 1, lift0 ListReverse)
                , ( 1, lift0 Optional)
                , ( 1, lift0 OptionalFold)
                , ( 1, lift0 OptionalBuild)
                , ( 1, lift1 Record)
                , ( 7, lift1 RecordLit)
                , ( 1, lift1 Union)
                , ( 7, lift2 Combine)
                , ( 1, lift2 CombineTypes)
                , ( 7, lift2 Prefer)
                , ( 1, lift3 Merge)
                , ( 1, lift2 ToMap)
                , ( 7, lift2 Field)
                , ( 7, lift2 Project)
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

instance Arbitrary Src where
    arbitrary = lift3 Src

    shrink = genericShrink

instance Arbitrary SourcePos where
    arbitrary = lift3 SourcePos

    shrink = genericShrink

instance Arbitrary Pos where
    arbitrary = lift1 (Megaparsec.mkPos . getPositive)

instance Arbitrary ImportType where
    arbitrary =
        Test.QuickCheck.oneof
            [ lift2 Local
            , lift5 (\a b c d e -> Remote (URL a b c d e))
            , lift1 Env
            , lift0 Missing
            ]

    shrink = genericShrink

instance Arbitrary ImportHashed where
    arbitrary =
        lift1 (ImportHashed Nothing)

    shrink (ImportHashed { importType = oldImportType, .. }) = do
        newImportType <- shrink oldImportType
        let importHashed = ImportHashed { importType = newImportType, .. }
        return importHashed

-- The standard does not yet specify how to encode `as Text`, so don't test it
-- yet
instance Arbitrary ImportMode where
    arbitrary = Test.QuickCheck.elements [ Code, RawText, Location ]

    shrink = genericShrink

instance Arbitrary Import where
    arbitrary = lift2 Import

    shrink = genericShrink

instance Arbitrary Scheme where
    arbitrary = Test.QuickCheck.oneof [ pure HTTP, pure HTTPS ]

    shrink = genericShrink

instance Arbitrary URL where
    arbitrary = lift5 URL

    shrink = genericShrink

instance Arbitrary Var where
    arbitrary =
        Test.QuickCheck.oneof
            [ fmap (V "_") (getNonNegative <$> arbitrary)
            , lift1 (\t -> V t 0)
            , lift1 V <*> (getNonNegative <$> arbitrary)
            ]

    shrink = genericShrink

binaryRoundtrip :: Expr () Import -> Property
binaryRoundtrip expression =
        wrap
            (fmap
                Dhall.Binary.decodeExpression
                (Codec.Serialise.deserialiseOrFail
                  (Codec.Serialise.serialise
                    (Dhall.Binary.encodeExpression expression)
                  )
                )
            )
    === wrap (Right (Right expression))
  where
    wrap
        :: Either DeserialiseFailure       a
        -> Either DeserialiseFailureWithEq a
    wrap = Data.Coerce.coerce

everythingWellTypedNormalizes :: Expr () () -> Property
everythingWellTypedNormalizes expression =
        isRight (Dhall.TypeCheck.typeWithA filterOutEmbeds Dhall.Context.empty expression)
    ==> Test.QuickCheck.total (Dhall.Core.normalize expression :: Expr () ())
  where
    filterOutEmbeds :: Typer a
    filterOutEmbeds _ = Const Sort -- This could be any ill-typed expression.

isNormalizedIsConsistentWithNormalize :: Expr () Import -> Property
isNormalizedIsConsistentWithNormalize expression =
    case maybeProp of
        Nothing -> Test.QuickCheck.discard
        Just prop -> prop
  where
      maybeProp = do
          nf <- Control.Spoon.spoon (Dhall.Core.normalize expression)
          isNormalized <- Control.Spoon.spoon (Dhall.Core.isNormalized expression)
          return $ isNormalized === (nf == expression)

normalizeWithMIsConsistentWithNormalize :: Expr () Import -> Property
normalizeWithMIsConsistentWithNormalize expression =
    case Control.Spoon.spoon (nfM, nf) of
        Just (a, b) -> a === b
        Nothing -> Test.QuickCheck.discard
  where nfM = runIdentity (Dhall.Core.normalizeWithM (\_ -> Identity Nothing) expression)
        nf = Dhall.Core.normalize expression :: Expr () Import

isSameAsSelf :: Expr () Import -> Property
isSameAsSelf expression =
  hasNoImportAndTypechecks ==> Dhall.Diff.same (Dhall.Diff.diff denoted denoted)
  where denoted = Dhall.Core.denote expression
        hasNoImportAndTypechecks =
          case traverse (\_ -> Left ()) expression of
            Right importlessExpression -> isRight (Dhall.TypeCheck.typeOf importlessExpression)
            Left _ -> False

inferredTypesAreNormalized :: Expr () Import -> Property
inferredTypesAreNormalized expression =
    Test.Tasty.QuickCheck.counterexample report (all Dhall.Core.isNormalized result)
  where
    report =  "Got: " ++ show result
           ++ "\nExpected: " ++ show (fmap Dhall.Core.normalize result
                                      :: Either (TypeError () Import) (Expr () Import))

    result = Dhall.TypeCheck.typeWithA filterOutEmbeds Dhall.Context.empty expression

    filterOutEmbeds :: Typer a
    filterOutEmbeds _ = Const Sort -- This could be any ill-typed expression.

tests :: TestTree
tests =
    testProperties'
        "QuickCheck"
        [ ( "Binary serialization should round-trip"
          , Test.QuickCheck.property binaryRoundtrip
          , QuickCheckTests 100
          )
        , ( "everything well-typed should normalize"
          , Test.QuickCheck.property everythingWellTypedNormalizes
          , QuickCheckTests 100000
          )
        , ( "isNormalized should be consistent with normalize"
          , Test.QuickCheck.property isNormalizedIsConsistentWithNormalize
          , QuickCheckTests 10000
          )
        , ( "normalizeWithM should be consistent with normalize"
          , Test.QuickCheck.property normalizeWithMIsConsistentWithNormalize
          , QuickCheckTests 10000
          )
        , ( "An expression should have no difference with itself"
          , Test.QuickCheck.property isSameAsSelf
          , QuickCheckTests 10000
          )
        , ( "Inferred types should be normalized"
          , Test.QuickCheck.property inferredTypesAreNormalized
          , QuickCheckTests 100000
          )
        ]

testProperties' :: String -> [(String, Property, QuickCheckTests)] -> TestTree
testProperties' name = Test.Tasty.testGroup name . map f
  where
    -- Using adjustOption instead of withMaxSuccess allows us to override the number of tests
    -- with the --quickcheck-tests CLI option.
    f (n, p, nTests) = Test.Tasty.adjustOption (max nTests) (Test.Tasty.QuickCheck.testProperty n p)
