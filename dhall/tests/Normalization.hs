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
        , customization
        , shouldNormalize
            "Optional build/fold fusion"
            "success/simple/optionalBuildFold"
        , shouldNormalize
            "a remote-systems.conf builder"
            "success/remoteSystems"
        , shouldNormalize
            "multi-line strings correctly"
            "success/simple/multiLine"
        , shouldNormalize
            "the // operator and sort the fields"
             "success/simple/sortOperator"
        , multiline
        ]

tutorialExamples :: TestTree
tutorialExamples =
    testGroup "Tutorial examples"
        [ shouldNormalize "⩓" "./success/haskell-tutorial/combineTypes/0"
        , shouldNormalize "//\\\\" "./success/haskell-tutorial/combineTypes/1"
        , shouldNormalize "//" "./success/haskell-tutorial/prefer/0"
        , shouldNormalize "projection" "./success/haskell-tutorial/projection/0"
        , shouldNormalize "access record" "./success/haskell-tutorial/access/0"
        , shouldNormalize "access union" "./success/haskell-tutorial/access/1"
        ]

preludeExamples :: TestTree
preludeExamples =
    testGroup "Prelude examples"
        [ shouldNormalize "Bool/and" "./success/prelude/Bool/and/0"
        , shouldNormalize "Bool/and" "./success/prelude/Bool/and/1"
        , shouldNormalize "Bool/build" "./success/prelude/Bool/build/0"
        , shouldNormalize "Bool/build" "./success/prelude/Bool/build/1"
        , shouldNormalize "Bool/even" "./success/prelude/Bool/even/0"
        , shouldNormalize "Bool/even" "./success/prelude/Bool/even/1"
        , shouldNormalize "Bool/even" "./success/prelude/Bool/even/2"
        , shouldNormalize "Bool/even" "./success/prelude/Bool/even/3"
        , shouldNormalize "Bool/fold" "./success/prelude/Bool/fold/0"
        , shouldNormalize "Bool/fold" "./success/prelude/Bool/fold/1"
        , shouldNormalize "Bool/not" "./success/prelude/Bool/not/0"
        , shouldNormalize "Bool/not" "./success/prelude/Bool/not/1"
        , shouldNormalize "Bool/odd" "./success/prelude/Bool/odd/0"
        , shouldNormalize "Bool/odd" "./success/prelude/Bool/odd/1"
        , shouldNormalize "Bool/odd" "./success/prelude/Bool/odd/2"
        , shouldNormalize "Bool/odd" "./success/prelude/Bool/odd/3"
        , shouldNormalize "Bool/or" "./success/prelude/Bool/or/0"
        , shouldNormalize "Bool/or" "./success/prelude/Bool/or/1"
        , shouldNormalize "Bool/show" "./success/prelude/Bool/show/0"
        , shouldNormalize "Bool/show" "./success/prelude/Bool/show/1"
        , shouldNormalize "Double/show" "./success/prelude/Double/show/0"
        , shouldNormalize "Double/show" "./success/prelude/Double/show/1"
        , shouldNormalize "Integer/show" "./success/prelude/Integer/show/0"
        , shouldNormalize "Integer/show" "./success/prelude/Integer/show/1"
        , shouldNormalize "Integer/toDouble" "./success/prelude/Integer/toDouble/0"
        , shouldNormalize "Integer/toDouble" "./success/prelude/Integer/toDouble/1"
        , shouldNormalize "List/all" "./success/prelude/List/all/0"
        , shouldNormalize "List/all" "./success/prelude/List/all/1"
        , shouldNormalize "List/any" "./success/prelude/List/any/0"
        , shouldNormalize "List/any" "./success/prelude/List/any/1"
        , shouldNormalize "List/build" "./success/prelude/List/build/0"
        , shouldNormalize "List/build" "./success/prelude/List/build/1"
        , shouldNormalize "List/concat" "./success/prelude/List/concat/0"
        , shouldNormalize "List/concat" "./success/prelude/List/concat/1"
        , shouldNormalize "List/concatMap" "./success/prelude/List/concatMap/0"
        , shouldNormalize "List/concatMap" "./success/prelude/List/concatMap/1"
        , shouldNormalize "List/filter" "./success/prelude/List/filter/0"
        , shouldNormalize "List/filter" "./success/prelude/List/filter/1"
        , shouldNormalize "List/fold" "./success/prelude/List/fold/0"
        , shouldNormalize "List/fold" "./success/prelude/List/fold/1"
        , shouldNormalize "List/fold" "./success/prelude/List/fold/2"
        , shouldNormalize "List/generate" "./success/prelude/List/generate/0"
        , shouldNormalize "List/generate" "./success/prelude/List/generate/1"
        , shouldNormalize "List/head" "./success/prelude/List/head/0"
        , shouldNormalize "List/head" "./success/prelude/List/head/1"
        , shouldNormalize "List/indexed" "./success/prelude/List/indexed/0"
        , shouldNormalize "List/indexed" "./success/prelude/List/indexed/1"
        , shouldNormalize "List/iterate" "./success/prelude/List/iterate/0"
        , shouldNormalize "List/iterate" "./success/prelude/List/iterate/1"
        , shouldNormalize "List/last" "./success/prelude/List/last/0"
        , shouldNormalize "List/last" "./success/prelude/List/last/1"
        , shouldNormalize "List/length" "./success/prelude/List/length/0"
        , shouldNormalize "List/length" "./success/prelude/List/length/1"
        , shouldNormalize "List/map" "./success/prelude/List/map/0"
        , shouldNormalize "List/map" "./success/prelude/List/map/1"
        , shouldNormalize "List/null" "./success/prelude/List/null/0"
        , shouldNormalize "List/null" "./success/prelude/List/null/1"
        , shouldNormalize "List/replicate" "./success/prelude/List/replicate/0"
        , shouldNormalize "List/replicate" "./success/prelude/List/replicate/1"
        , shouldNormalize "List/reverse" "./success/prelude/List/reverse/0"
        , shouldNormalize "List/reverse" "./success/prelude/List/reverse/1"
        , shouldNormalize "List/shifted" "./success/prelude/List/shifted/0"
        , shouldNormalize "List/shifted" "./success/prelude/List/shifted/1"
        , shouldNormalize "List/unzip" "./success/prelude/List/unzip/0"
        , shouldNormalize "List/unzip" "./success/prelude/List/unzip/1"
        , shouldNormalize "Natural/build" "./success/prelude/Natural/build/0"
        , shouldNormalize "Natural/build" "./success/prelude/Natural/build/1"
        , shouldNormalize "Natural/enumerate" "./success/prelude/Natural/enumerate/0"
        , shouldNormalize "Natural/enumerate" "./success/prelude/Natural/enumerate/1"
        , shouldNormalize "Natural/even" "./success/prelude/Natural/even/0"
        , shouldNormalize "Natural/even" "./success/prelude/Natural/even/1"
        , shouldNormalize "Natural/fold" "./success/prelude/Natural/fold/0"
        , shouldNormalize "Natural/fold" "./success/prelude/Natural/fold/1"
        , shouldNormalize "Natural/fold" "./success/prelude/Natural/fold/2"
        , shouldNormalize "Natural/isZero" "./success/prelude/Natural/isZero/0"
        , shouldNormalize "Natural/isZero" "./success/prelude/Natural/isZero/1"
        , shouldNormalize "Natural/odd" "./success/prelude/Natural/odd/0"
        , shouldNormalize "Natural/odd" "./success/prelude/Natural/odd/1"
        , shouldNormalize "Natural/product" "./success/prelude/Natural/product/0"
        , shouldNormalize "Natural/product" "./success/prelude/Natural/product/1"
        , shouldNormalize "Natural/show" "./success/prelude/Natural/show/0"
        , shouldNormalize "Natural/show" "./success/prelude/Natural/show/1"
        , shouldNormalize "Natural/sum" "./success/prelude/Natural/sum/0"
        , shouldNormalize "Natural/sum" "./success/prelude/Natural/sum/1"
        , shouldNormalize "Natural/toDouble" "./success/prelude/Natural/toDouble/0"
        , shouldNormalize "Natural/toDouble" "./success/prelude/Natural/toDouble/1"
        , shouldNormalize "Natural/toInteger" "./success/prelude/Natural/toInteger/0"
        , shouldNormalize "Natural/toInteger" "./success/prelude/Natural/toInteger/1"
        , shouldNormalize "Optional/all" "./success/prelude/Optional/all/0"
        , shouldNormalize "Optional/all" "./success/prelude/Optional/all/1"
        , shouldNormalize "Optional/any" "./success/prelude/Optional/any/0"
        , shouldNormalize "Optional/any" "./success/prelude/Optional/any/1"
        , shouldNormalize "Optional/build" "./success/prelude/Optional/build/0"
        , shouldNormalize "Optional/build" "./success/prelude/Optional/build/1"
        , shouldNormalize "Optional/concat" "./success/prelude/Optional/concat/0"
        , shouldNormalize "Optional/concat" "./success/prelude/Optional/concat/1"
        , shouldNormalize "Optional/concat" "./success/prelude/Optional/concat/2"
        , shouldNormalize "Optional/filter" "./success/prelude/Optional/filter/0"
        , shouldNormalize "Optional/filter" "./success/prelude/Optional/filter/1"
        , shouldNormalize "Optional/fold" "./success/prelude/Optional/fold/0"
        , shouldNormalize "Optional/fold" "./success/prelude/Optional/fold/1"
        , shouldNormalize "Optional/head" "./success/prelude/Optional/head/0"
        , shouldNormalize "Optional/head" "./success/prelude/Optional/head/1"
        , shouldNormalize "Optional/head" "./success/prelude/Optional/head/2"
        , shouldNormalize "Optional/last" "./success/prelude/Optional/last/0"
        , shouldNormalize "Optional/last" "./success/prelude/Optional/last/1"
        , shouldNormalize "Optional/last" "./success/prelude/Optional/last/2"
        , shouldNormalize "Optional/length" "./success/prelude/Optional/length/0"
        , shouldNormalize "Optional/length" "./success/prelude/Optional/length/1"
        , shouldNormalize "Optional/map" "./success/prelude/Optional/map/0"
        , shouldNormalize "Optional/map" "./success/prelude/Optional/map/1"
        , shouldNormalize "Optional/null" "./success/prelude/Optional/null/0"
        , shouldNormalize "Optional/null" "./success/prelude/Optional/null/1"
        , shouldNormalize "Optional/toList" "./success/prelude/Optional/toList/0"
        , shouldNormalize "Optional/toList" "./success/prelude/Optional/toList/1"
        , shouldNormalize "Optional/unzip" "./success/prelude/Optional/unzip/0"
        , shouldNormalize "Optional/unzip" "./success/prelude/Optional/unzip/1"
        , shouldNormalize "Text/concat" "./success/prelude/Text/concat/0"
        , shouldNormalize "Text/concat" "./success/prelude/Text/concat/1"
        , shouldNormalize "Text/concatMap" "./success/prelude/Text/concatMap/0"
        , shouldNormalize "Text/concatMap" "./success/prelude/Text/concatMap/1"
        , shouldNormalize "Text/concatMapSep" "./success/prelude/Text/concatMapSep/0"
        , shouldNormalize "Text/concatMapSep" "./success/prelude/Text/concatMapSep/1"
        , shouldNormalize "Text/concatSep" "./success/prelude/Text/concatSep/0"
        , shouldNormalize "Text/concatSep" "./success/prelude/Text/concatSep/1"
        ]

simplifications :: TestTree
simplifications =
    testGroup "Simplifications"
        [ shouldNormalize "if/then/else" "./success/simplifications/ifThenElse"
        , shouldNormalize "||" "./success/simplifications/or"
        , shouldNormalize "&&" "./success/simplifications/and"
        , shouldNormalize "==" "./success/simplifications/eq"
        , shouldNormalize "!=" "./success/simplifications/ne"
        ]

constantFolding :: TestTree
constantFolding =
    testGroup "folding of constants"
        [ shouldNormalize "Natural/plus"   "success/simple/naturalPlus"
        , shouldNormalize "Optional/fold"  "success/simple/optionalFold"
        , shouldNormalize "Optional/build" "success/simple/optionalBuild"
        , shouldNormalize "Natural/build"  "success/simple/naturalBuild"
        ]

conversions :: TestTree
conversions =
    testGroup "conversions"
        [ shouldNormalize "Natural/show" "success/simple/naturalShow"
        , shouldNormalize "Integer/show" "success/simple/integerShow"
        , shouldNormalize "Double/show"  "success/simple/doubleShow"
        , shouldNormalize "Natural/toInteger" "success/simple/naturalToInteger"
        , shouldNormalize "Integer/toDouble" "success/simple/integerToDouble"
        ]

customization :: TestTree
customization =
    testGroup "customization"
        [ simpleCustomization
        , nestedReduction
        ]

multiline :: TestTree
multiline =
    testGroup "Multi-line literals"
        [ shouldNormalize
            "multi-line escape sequences"
            "./success/multiline/escape"
        , shouldNormalize
            "a multi-line literal with a hanging indent"
            "./success/multiline/hangingIndent"
        , shouldNormalize
            "a multi-line literal with an interior indent"
            "./success/multiline/interiorIndent"
        , shouldNormalize
            "a multi-line literal with an interpolated expression"
            "./success/multiline/interpolation"
        , should
            "preserve comments within a multi-line literal"
            "./success/multiline/preserveComment"
        , shouldNormalize
            "a multi-line literal with one line"
            "./success/multiline/singleLine"
        , shouldNormalize
            "a multi-line literal with two lines"
            "./success/multiline/twoLines"
        ]

simpleCustomization :: TestTree
simpleCustomization = testCase "simpleCustomization" $ do
  let tyCtx  = insert "min" (Pi "_" Natural (Pi "_" Natural Natural)) empty
      valCtx e = case e of
                    (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y)) -> pure (Just (NaturalLit (min x y)))
                    _ -> pure Nothing
  e <- codeWith tyCtx "min (min 11 12) 8 + 1"
  assertNormalizesToWith valCtx e "9"

nestedReduction :: TestTree
nestedReduction = testCase "doubleReduction" $ do
  minType        <- insert "min"        <$> code "Natural → Natural → Natural"
  fiveorlessType <- insert "fiveorless" <$> code "Natural → Natural"
  wurbleType     <- insert "wurble"     <$> code "Natural → Integer"
  let tyCtx = minType . fiveorlessType . wurbleType $ empty
      valCtx e = case e of
                    (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y)) -> pure (Just (NaturalLit (min x y)))
                    (App (Var (V "wurble" 0)) (NaturalLit x)) -> pure (Just
                        (App (Var (V "fiveorless" 0)) (NaturalPlus (NaturalLit x) (NaturalLit 2))))
                    (App (Var (V "fiveorless" 0)) (NaturalLit x)) -> pure (Just
                        (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalPlus (NaturalLit 3) (NaturalLit 2))))
                    _ -> pure Nothing
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
        let actualNormalized =
                Dhall.Core.alphaNormalize
                    (Dhall.Core.normalize actualResolved :: Expr X X)

        expectedExpr <- case Dhall.Parser.exprFromText mempty expectedCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        expectedResolved <- Dhall.Import.load expectedExpr
        case Dhall.TypeCheck.typeOf expectedResolved of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()
        -- Use `denote` instead of `normalize` to enforce that the expected
        -- expression is already in normal form
        let expectedNormalized =
                Dhall.Core.alphaNormalize (Dhall.Core.denote expectedResolved)

        let message =
                "The normalized expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expectedNormalized actualNormalized

shouldNormalize :: Text -> Text -> TestTree
shouldNormalize name = should ("normalize " <> name <> " correctly")
