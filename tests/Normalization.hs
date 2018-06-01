{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Normalization (normalizationTests) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)

import qualified Control.Exception
import qualified Data.Text
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck

import Dhall.Core
import Dhall.Context
import Test.Tasty
import Test.Tasty.HUnit
import Util

normalizationTests :: TestTree
normalizationTests =
    testGroup "normalization"
        [ tutorialExamples
        , preludeExamples
        , simplifications
        , constantFolding
        , conversions
        , shouldNormalize "Optional build/fold fusion" "optionalBuildFold"
        , customization
        , shouldNormalize "a remote-systems.conf builder" "remoteSystems"
        ]

tutorialExamples :: TestTree
tutorialExamples =
    testGroup "Tutorial examples"
        [ shouldNormalize "⩓" "./tutorial/combineTypes/0"
        , shouldNormalize "projection" "./tutorial/projection/0"
        ]

preludeExamples :: TestTree
preludeExamples =
    testGroup "Prelude examples"
        [ shouldNormalize "Bool/and" "./examples/Bool/and/0"
        , shouldNormalize "Bool/and" "./examples/Bool/and/1"
        , shouldNormalize "Bool/build" "./examples/Bool/build/0"
        , shouldNormalize "Bool/build" "./examples/Bool/build/1"
        , shouldNormalize "Bool/even" "./examples/Bool/even/0"
        , shouldNormalize "Bool/even" "./examples/Bool/even/1"
        , shouldNormalize "Bool/even" "./examples/Bool/even/2"
        , shouldNormalize "Bool/even" "./examples/Bool/even/3"
        , shouldNormalize "Bool/fold" "./examples/Bool/fold/0"
        , shouldNormalize "Bool/fold" "./examples/Bool/fold/1"
        , shouldNormalize "Bool/not" "./examples/Bool/not/0"
        , shouldNormalize "Bool/not" "./examples/Bool/not/1"
        , shouldNormalize "Bool/odd" "./examples/Bool/odd/0"
        , shouldNormalize "Bool/odd" "./examples/Bool/odd/1"
        , shouldNormalize "Bool/odd" "./examples/Bool/odd/2"
        , shouldNormalize "Bool/odd" "./examples/Bool/odd/3"
        , shouldNormalize "Bool/or" "./examples/Bool/or/0"
        , shouldNormalize "Bool/or" "./examples/Bool/or/1"
        , shouldNormalize "Bool/show" "./examples/Bool/show/0"
        , shouldNormalize "Bool/show" "./examples/Bool/show/1"
        , shouldNormalize "Double/show" "./examples/Double/show/0"
        , shouldNormalize "Double/show" "./examples/Double/show/1"
        , shouldNormalize "Integer/show" "./examples/Integer/show/0"
        , shouldNormalize "Integer/show" "./examples/Integer/show/1"
        , shouldNormalize "List/all" "./examples/List/all/0"
        , shouldNormalize "List/all" "./examples/List/all/1"
        , shouldNormalize "List/any" "./examples/List/any/0"
        , shouldNormalize "List/any" "./examples/List/any/1"
        , shouldNormalize "List/build" "./examples/List/build/0"
        , shouldNormalize "List/build" "./examples/List/build/1"
        , shouldNormalize "List/concat" "./examples/List/concat/0"
        , shouldNormalize "List/concat" "./examples/List/concat/1"
        , shouldNormalize "List/concatMap" "./examples/List/concatMap/0"
        , shouldNormalize "List/concatMap" "./examples/List/concatMap/1"
        , shouldNormalize "List/filter" "./examples/List/filter/0"
        , shouldNormalize "List/filter" "./examples/List/filter/1"
        , shouldNormalize "List/fold" "./examples/List/fold/0"
        , shouldNormalize "List/fold" "./examples/List/fold/1"
        , shouldNormalize "List/fold" "./examples/List/fold/2"
        , shouldNormalize "List/generate" "./examples/List/generate/0"
        , shouldNormalize "List/generate" "./examples/List/generate/1"
        , shouldNormalize "List/head" "./examples/List/head/0"
        , shouldNormalize "List/head" "./examples/List/head/1"
        , shouldNormalize "List/indexed" "./examples/List/indexed/0"
        , shouldNormalize "List/indexed" "./examples/List/indexed/1"
        , shouldNormalize "List/iterate" "./examples/List/iterate/0"
        , shouldNormalize "List/iterate" "./examples/List/iterate/1"
        , shouldNormalize "List/last" "./examples/List/last/0"
        , shouldNormalize "List/last" "./examples/List/last/1"
        , shouldNormalize "List/length" "./examples/List/length/0"
        , shouldNormalize "List/length" "./examples/List/length/1"
        , shouldNormalize "List/map" "./examples/List/map/0"
        , shouldNormalize "List/map" "./examples/List/map/1"
        , shouldNormalize "List/null" "./examples/List/null/0"
        , shouldNormalize "List/null" "./examples/List/null/1"
        , shouldNormalize "List/replicate" "./examples/List/replicate/0"
        , shouldNormalize "List/replicate" "./examples/List/replicate/1"
        , shouldNormalize "List/reverse" "./examples/List/reverse/0"
        , shouldNormalize "List/reverse" "./examples/List/reverse/1"
        , shouldNormalize "List/shifted" "./examples/List/shifted/0"
        , shouldNormalize "List/shifted" "./examples/List/shifted/1"
        , shouldNormalize "List/unzip" "./examples/List/unzip/0"
        , shouldNormalize "List/unzip" "./examples/List/unzip/1"
        , shouldNormalize "Natural/build" "./examples/Natural/build/0"
        , shouldNormalize "Natural/build" "./examples/Natural/build/1"
        , shouldNormalize "Natural/enumerate" "./examples/Natural/enumerate/0"
        , shouldNormalize "Natural/enumerate" "./examples/Natural/enumerate/1"
        , shouldNormalize "Natural/even" "./examples/Natural/even/0"
        , shouldNormalize "Natural/even" "./examples/Natural/even/1"
        , shouldNormalize "Natural/fold" "./examples/Natural/fold/0"
        , shouldNormalize "Natural/fold" "./examples/Natural/fold/1"
        , shouldNormalize "Natural/fold" "./examples/Natural/fold/2"
        , shouldNormalize "Natural/isZero" "./examples/Natural/isZero/0"
        , shouldNormalize "Natural/isZero" "./examples/Natural/isZero/1"
        , shouldNormalize "Natural/odd" "./examples/Natural/odd/0"
        , shouldNormalize "Natural/odd" "./examples/Natural/odd/1"
        , shouldNormalize "Natural/product" "./examples/Natural/product/0"
        , shouldNormalize "Natural/product" "./examples/Natural/product/1"
        , shouldNormalize "Natural/show" "./examples/Natural/show/0"
        , shouldNormalize "Natural/show" "./examples/Natural/show/1"
        , shouldNormalize "Natural/sum" "./examples/Natural/sum/0"
        , shouldNormalize "Natural/sum" "./examples/Natural/sum/1"
        , shouldNormalize "Natural/toInteger" "./examples/Natural/toInteger/0"
        , shouldNormalize "Natural/toInteger" "./examples/Natural/toInteger/1"
        , shouldNormalize "Optional/all" "./examples/Optional/all/0"
        , shouldNormalize "Optional/all" "./examples/Optional/all/1"
        , shouldNormalize "Optional/any" "./examples/Optional/any/0"
        , shouldNormalize "Optional/any" "./examples/Optional/any/1"
        , shouldNormalize "Optional/build" "./examples/Optional/build/0"
        , shouldNormalize "Optional/build" "./examples/Optional/build/1"
        , shouldNormalize "Optional/concat" "./examples/Optional/concat/0"
        , shouldNormalize "Optional/concat" "./examples/Optional/concat/1"
        , shouldNormalize "Optional/concat" "./examples/Optional/concat/2"
        , shouldNormalize "Optional/filter" "./examples/Optional/filter/0"
        , shouldNormalize "Optional/filter" "./examples/Optional/filter/1"
        , shouldNormalize "Optional/fold" "./examples/Optional/fold/0"
        , shouldNormalize "Optional/fold" "./examples/Optional/fold/1"
        , shouldNormalize "Optional/head" "./examples/Optional/head/0"
        , shouldNormalize "Optional/head" "./examples/Optional/head/1"
        , shouldNormalize "Optional/last" "./examples/Optional/last/0"
        , shouldNormalize "Optional/last" "./examples/Optional/last/1"
        , shouldNormalize "Optional/length" "./examples/Optional/length/0"
        , shouldNormalize "Optional/length" "./examples/Optional/length/1"
        , shouldNormalize "Optional/map" "./examples/Optional/map/0"
        , shouldNormalize "Optional/map" "./examples/Optional/map/1"
        , shouldNormalize "Optional/null" "./examples/Optional/null/0"
        , shouldNormalize "Optional/null" "./examples/Optional/null/1"
        , shouldNormalize "Optional/toList" "./examples/Optional/toList/0"
        , shouldNormalize "Optional/toList" "./examples/Optional/toList/1"
        , shouldNormalize "Optional/unzip" "./examples/Optional/unzip/0"
        , shouldNormalize "Optional/unzip" "./examples/Optional/unzip/1"
        , shouldNormalize "Text/concat" "./examples/Text/concat/0"
        , shouldNormalize "Text/concat" "./examples/Text/concat/1"
        , shouldNormalize "Text/concatMap" "./examples/Text/concatMap/0"
        , shouldNormalize "Text/concatMap" "./examples/Text/concatMap/1"
        , shouldNormalize "Text/concatMapSep" "./examples/Text/concatMapSep/0"
        , shouldNormalize "Text/concatMapSep" "./examples/Text/concatMapSep/1"
        , shouldNormalize "Text/concatSep" "./examples/Text/concatSep/0"
        , shouldNormalize "Text/concatSep" "./examples/Text/concatSep/1"
        ]

simplifications :: TestTree
simplifications =
    testGroup "Simplifications"
        [ shouldNormalize "if/then/else" "./simplifications/ifThenElse"
        , shouldNormalize "||" "./simplifications/or"
        , shouldNormalize "&&" "./simplifications/and"
        , shouldNormalize "==" "./simplifications/eq"
        , shouldNormalize "!=" "./simplifications/ne"
        ]

constantFolding :: TestTree
constantFolding =
    testGroup "folding of constants"
        [ shouldNormalize "Natural/plus"   "naturalPlus"
        , shouldNormalize "Optional/fold"  "optionalFold"
        , shouldNormalize "Optional/build" "optionalBuild"
        ]

conversions :: TestTree
conversions =
    testGroup "conversions"
        [ shouldNormalize "Natural/show" "naturalShow"
        , shouldNormalize "Integer/show" "integerShow"
        , shouldNormalize "Double/show"  "doubleShow"
        , shouldNormalize "Natural/toInteger" "naturalToInteger"
        ]

customization :: TestTree
customization =
    testGroup "customization"
        [ simpleCustomization
        , nestedReduction
        ]

simpleCustomization :: TestTree
simpleCustomization = testCase "simpleCustomization" $ do
  let tyCtx  = insert "min" (Pi "_" Natural (Pi "_" Natural Natural)) empty
      valCtx e = case e of
                    (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y)) -> Just (NaturalLit (min x y))
                    _ -> Nothing
  e <- codeWith tyCtx "min (min 11 12) 8 + 1"
  assertNormalizesToWith valCtx e "9"

nestedReduction :: TestTree
nestedReduction = testCase "doubleReduction" $ do
  minType        <- insert "min"        <$> code "Natural → Natural → Natural"
  fiveorlessType <- insert "fiveorless" <$> code "Natural → Natural"
  wurbleType     <- insert "wurble"     <$> code "Natural → Integer"
  let tyCtx = minType . fiveorlessType . wurbleType $ empty
      valCtx e = case e of
                    (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y)) -> Just (NaturalLit (min x y))
                    (App (Var (V "wurble" 0)) (NaturalLit x)) -> Just
                        (App (Var (V "fiveorless" 0)) (NaturalPlus (NaturalLit x) (NaturalLit 2)))
                    (App (Var (V "fiveorless" 0)) (NaturalLit x)) -> Just
                        (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalPlus (NaturalLit 3) (NaturalLit 2)))
                    _ -> Nothing
  e <- codeWith tyCtx "wurble 6"
  assertNormalizesToWith valCtx e "5"

should :: Text -> Text -> TestTree
should name basename =
    Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
        let actualCode   = "./tests/normalization/" <> basename <> "A.dhall"
        let expectedCode = "./tests/normalization/" <> basename <> "B.dhall"

        actualExpr <- case Dhall.Parser.exprFromText mempty actualCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        actualResolved <- Dhall.Import.load actualExpr
        case Dhall.TypeCheck.typeOf actualResolved of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()
        let actualNormalized = Dhall.Core.normalize actualResolved :: Expr X X

        expectedExpr <- case Dhall.Parser.exprFromText mempty expectedCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        expectedResolved <- Dhall.Import.load expectedExpr
        case Dhall.TypeCheck.typeOf expectedResolved of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()
        -- Use `denote` instead of `normalize` to enforce that the expected
        -- expression is already in normal form
        let expectedNormalized = Dhall.Core.denote expectedResolved

        let message =
                "The normalized expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expectedNormalized actualNormalized

shouldNormalize :: Text -> Text -> TestTree
shouldNormalize name = should ("normalize " <> name <> " correctly")
