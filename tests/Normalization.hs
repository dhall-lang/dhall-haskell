{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Normalization (normalizationTests) where

import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)

import qualified Control.Exception
import qualified Data.Text.Lazy
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
        [ constantFolding
        , conversions
        , shouldNormalize "Optional build/fold fusion" "optionalBuildFold"
        , customization
        , shouldNormalize "a remote-systems.conf builder" "remoteSystems"
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
  e <- codeWith tyCtx "min (min +11 +12) +8 + +1" 
  assertNormalizesToWith valCtx e "+9"

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
  e <- codeWith tyCtx "wurble +6"
  assertNormalizesToWith valCtx e "+5"

should :: Text -> Text -> TestTree
should name basename =
    Test.Tasty.HUnit.testCase (Data.Text.Lazy.unpack name) $ do
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
