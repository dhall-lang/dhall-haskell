{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Test.QuickCheck where

import Codec.Serialise (DeserialiseFailure(..))
import Data.Either (isRight)
import Dhall (Inject(..), Interpret(..), auto, input, inject, embed, Vector)
import Dhall.Map (Map)
import Dhall.Core
    ( Binding(..)
    , Chunks(..)
    , Const(..)
    , Directory(..)
    , DhallDouble(..)
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
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))
import Dhall.Set (Set)
import Dhall.Src (Src(..))
import Dhall.Pretty (prettyExpr)
import Dhall.TypeCheck (Typer, TypeError)
import Generic.Random (Weights, W, (%), (:+)(..))
import Test.QuickCheck
    (Arbitrary(..), Gen, Positive(..), Property, NonNegative(..), genericShrink, (===), (==>))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (QuickCheckTests(..))
import Text.Megaparsec (SourcePos(..), Pos)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Control.Spoon
import qualified Codec.Serialise
import qualified Data.Coerce
import qualified Data.List
import qualified Data.Sequence
import qualified Data.SpecialValues
import qualified Data.HashSet
import qualified Data.Set
import qualified Data.Text as Text
import qualified Data.Map
import qualified Dhall.Binary
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Diff
import qualified Dhall.Map
import qualified Dhall.Set
import qualified Dhall.TypeCheck
import qualified Generic.Random
import qualified GHC.Natural as GHCNat
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

instance (Arbitrary s, Arbitrary a) => Arbitrary (Binding s a) where
    arbitrary =
        let adapt = fmap ((,) Nothing)
            f a b   = Binding Nothing "_" Nothing (adapt a) Nothing b
            g a b c = Binding Nothing a   Nothing (adapt b) Nothing c

        in  Test.QuickCheck.oneof [ lift2 f, lift3 g ]

    shrink = genericShrink

instance (Arbitrary s, Arbitrary a) => Arbitrary (Chunks s a) where
    arbitrary = do
        n <- Test.QuickCheck.choose (0, 2)
        Chunks <$> Test.QuickCheck.vectorOf n arbitrary <*> arbitrary

    shrink = genericShrink

instance Arbitrary Const where
    arbitrary = Test.QuickCheck.oneof [ pure Type, pure Kind, pure Sort ]

    shrink = genericShrink

instance Arbitrary DhallDouble where
    arbitrary = fmap DhallDouble (Test.QuickCheck.oneof [ arbitrary, special ])
      where
        special = Test.QuickCheck.elements Data.SpecialValues.specialValues

    shrink = genericShrink

instance Arbitrary Directory where
    arbitrary = lift1 Directory

    shrink = genericShrink

instance (Arbitrary s, Arbitrary a) => Arbitrary (Expr s a) where
    arbitrary =
        Test.QuickCheck.suchThat
            (Generic.Random.genericArbitraryRecG customGens weights)
            standardizedExpression
      where
        customGens
            :: Gen Integer    -- Generates all Integer fields in Expr
            :+ Gen Text.Text  -- Generates all Text fields in Expr
            :+ ()
        customGens =
               integer
               -- 'Lam's and 'Pi's are encoded differently when the binding is
               -- the special string "_", so we generate some of these strings
               -- to improve test coverage for these code paths.
            :+ Test.QuickCheck.oneof [pure "_", arbitrary]
            :+ ()

        -- These weights determine the frequency of constructors in the generated
        -- Expr.
        -- They will fail to compile if the constructors don't appear in the order
        -- in which they are defined in 'Expr'!
        weights :: Weights (Expr s a)
        weights =
              (7 :: W "Const")
            % (7 :: W "Var")
            % (7 :: W "Lam")
            % (7 :: W "Pi")
            % (7 :: W "App")
            % (7 :: W "Let")
            % (1 :: W "Annot")
            % (1 :: W "Bool")
            % (7 :: W "BoolLit")
            % (1 :: W "BoolAnd")
            % (1 :: W "BoolOr")
            % (1 :: W "BoolEQ")
            % (1 :: W "BoolNE")
            % (1 :: W "BoolIf")
            % (1 :: W "Natural")
            % (7 :: W "NaturalLit")
            % (1 :: W "NaturalFold")
            % (1 :: W "NaturalBuild")
            % (1 :: W "NaturalIsZero")
            % (1 :: W "NaturalEven")
            % (1 :: W "NaturalOdd")
            % (1 :: W "NaturalToInteger")
            % (1 :: W "NaturalShow")
            % (1 :: W "NaturalSubtract")
            % (1 :: W "NaturalPlus")
            % (1 :: W "NaturalTimes")
            % (1 :: W "Integer")
            % (7 :: W "IntegerLit")
            % (1 :: W "IntegerShow")
            % (1 :: W "IntegerToDouble")
            % (1 :: W "Double")
            % (7 :: W "DoubleLit")
            % (1 :: W "DoubleShow")
            % (1 :: W "Text")
            % (1 :: W "TextLit")
            % (1 :: W "TextAppend")
            % (1 :: W "TextShow")
            % (1 :: W "List")
            % (1 :: W "ListLit")
            % (1 :: W "ListAppend")
            % (1 :: W "ListBuild")
            % (1 :: W "ListFold")
            % (1 :: W "ListLength")
            % (1 :: W "ListHead")
            % (1 :: W "ListLast")
            % (1 :: W "ListIndexed")
            % (1 :: W "ListReverse")
            % (1 :: W "Optional")
            % (7 :: W "Some")
            % (1 :: W "None")
            % (1 :: W "OptionalFold")
            % (1 :: W "OptionalBuild")
            % (1 :: W "Record")
            % (7 :: W "RecordLit")
            % (1 :: W "Union")
            % (7 :: W "Combine")
            % (1 :: W "CombineTypes")
            % (7 :: W "Prefer")
            % (1 :: W "Merge")
            % (1 :: W "ToMap")
            % (7 :: W "Field")
            % (7 :: W "Project")
            % (1 :: W "Assert")
            % (1 :: W "Equivalent")
            % (0 :: W "Note")
            % (7 :: W "ImportAlt")
            % (7 :: W "Embed")
            % ()

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
    === wrap (Right (Right (Dhall.Core.denote expression :: Expr () Import)))
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

normalizingAnExpressionDoesntChangeItsInferredType :: Expr () Import -> Property
normalizingAnExpressionDoesntChangeItsInferredType expression =
    case (eT0, eT1) of
        (Right t0, Right t1) -> t0 === t1
        _ -> Test.QuickCheck.discard
  where
    eT0 = typeCheck expression
    eT1 = typeCheck (Dhall.Core.normalize expression)

    typeCheck = Dhall.TypeCheck.typeWithA filterOutEmbeds Dhall.Context.empty

    filterOutEmbeds :: Typer a
    filterOutEmbeds _ = Const Sort -- This could be any ill-typed expression.

injectThenInterpretIsIdentity
    :: forall a. (Inject a, Interpret a, Eq a, Typeable a, Arbitrary a, Show a)
    => Proxy a
    -> (String, Property, QuickCheckTests)
injectThenInterpretIsIdentity p =
    ( "Injecting then Interpreting is identity for " <> show (typeRep p)
    , Test.QuickCheck.property (prop :: a -> Property)
    , QuickCheckTests 1000
    )
  where
    prop a = monadicIO $ do
        a' <- run . input auto . Text.pack . show . prettyExpr . embed inject $ a
        assert (a == a')


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
          , QuickCheckTests 10000
          )
        , ( "Normalizing an expression doesn't change its inferred type"
          , Test.QuickCheck.property normalizingAnExpressionDoesntChangeItsInferredType
          , QuickCheckTests 10000
          )
        , injectThenInterpretIsIdentity (Proxy :: Proxy [GHCNat.Natural])
        , injectThenInterpretIsIdentity (Proxy :: Proxy (Bool, Double))
        , injectThenInterpretIsIdentity (Proxy :: Proxy (Data.Sequence.Seq ()))
        , injectThenInterpretIsIdentity (Proxy :: Proxy (Maybe Integer))
        , injectThenInterpretIsIdentity (Proxy :: Proxy (Data.Set.Set GHCNat.Natural))
        , injectThenInterpretIsIdentity (Proxy :: Proxy (Data.HashSet.HashSet Double))
        , injectThenInterpretIsIdentity (Proxy :: Proxy (Vector Double))
        , injectThenInterpretIsIdentity (Proxy :: Proxy (Data.Map.Map Double Bool))
        ]



testProperties' :: String -> [(String, Property, QuickCheckTests)] -> TestTree
testProperties' name = Test.Tasty.testGroup name . map f
  where
    -- Using adjustOption instead of withMaxSuccess allows us to override the number of tests
    -- with the --quickcheck-tests CLI option.
    f (n, p, nTests) = Test.Tasty.adjustOption (max nTests) (Test.Tasty.QuickCheck.testProperty n p)
