{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Normalization where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Void (Void)
import Dhall.Core (Expr(..), Var(..), throws)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text.IO
import qualified Dhall.Context    as Context
import qualified Dhall.Core       as Core
import qualified Dhall.Import     as Import
import qualified Dhall.Parser     as Parser
import qualified Dhall.Test.Util  as Test.Util
import qualified Dhall.TypeCheck  as TypeCheck
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Turtle

normalizationDirectory :: FilePath
normalizationDirectory = "./dhall-lang/tests/normalization/success"

getTests :: IO TestTree
getTests = do
    let pattern = Turtle.chars <* "A.dhall"

    let normalizationFiles = do
            path <- Turtle.lstree normalizationDirectory

            Nothing <- return (Turtle.stripPrefix (normalizationDirectory </> "unit/") path)

            return path

    betaNormalizationTests <- Test.Util.discover pattern betaNormalizationTest normalizationFiles

    alphaNormalizationTests <- do
        Test.Util.discover pattern alphaNormalizationTest
            (Turtle.lstree "./dhall-lang/tests/alpha-normalization/success/")

    let unitTestFiles = Turtle.lstree (normalizationDirectory </> "unit/")

    unitTests <- Test.Util.discover pattern unitTest unitTestFiles

    let testTree =
            Tasty.testGroup "normalization"
                [ betaNormalizationTests
                , unitTests
                , alphaNormalizationTests
                , customization
                ]

    return testTree

customization :: TestTree
customization =
    Tasty.testGroup "customization"
        [ simpleCustomization
        , nestedReduction
        ]

simpleCustomization :: TestTree
simpleCustomization = Tasty.HUnit.testCase "simpleCustomization" $ do
    let tyCtx =
            Context.insert
                "min"
                (Pi "_" Natural (Pi "_" Natural Natural))
                Context.empty

        valCtx e =
            case e of
                App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y) ->
                    pure (Just (NaturalLit (min x y)))
                _ ->
                    pure Nothing

    e <- Test.Util.codeWith tyCtx "min (min 11 12) 8 + 1"

    Test.Util.assertNormalizesToWith valCtx e "9"

nestedReduction :: TestTree
nestedReduction = Tasty.HUnit.testCase "doubleReduction" $ do
    minType        <- Context.insert "min"        <$> Test.Util.code "Natural → Natural → Natural"
    fiveorlessType <- Context.insert "fiveorless" <$> Test.Util.code "Natural → Natural"
    wurbleType     <- Context.insert "wurble"     <$> Test.Util.code "Natural → Natural"

    let tyCtx = minType . fiveorlessType . wurbleType $ Context.empty

        valCtx e =
            case e of
                App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y) ->
                    pure (Just (NaturalLit (min x y)))
                App (Var (V "wurble" 0)) (NaturalLit x) ->
                    pure (Just (App (Var (V "fiveorless" 0)) (NaturalPlus (NaturalLit x) (NaturalLit 2))))
                App (Var (V "fiveorless" 0)) (NaturalLit x) ->
                    pure (Just (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalPlus (NaturalLit 3) (NaturalLit 2))))
                _ ->
                    pure Nothing

    e <- Test.Util.codeWith tyCtx "wurble 6"

    Test.Util.assertNormalizesToWith valCtx e "5"

alphaNormalizationTest :: Text -> TestTree
alphaNormalizationTest prefix = do
    let prefixString = Text.unpack prefix

    Tasty.HUnit.testCase prefixString $ do
        let actualPath   = prefixString <> "A.dhall"
        let expectedPath = prefixString <> "B.dhall"

        actualCode   <- Text.IO.readFile actualPath
        expectedCode <- Text.IO.readFile expectedPath

        actualExpr <- throws (Parser.exprFromText mempty actualCode)

        actualResolved <- Import.assertNoImports actualExpr

        let actualNormalized = Core.alphaNormalize (Core.denote actualResolved)

        expectedExpr <- throws (Parser.exprFromText mempty expectedCode)

        expectedResolved <- Import.assertNoImports expectedExpr

        let expectedNormalized = Core.denote expectedResolved :: Expr Void Void

        let message =
                "The normalized expression did not match the expected output"

        Tasty.HUnit.assertEqual message expectedNormalized actualNormalized

{- Unit tests don't type-check, so we only verify that they normalize to the
   expected output
-}
unitTest :: Text -> TestTree
unitTest prefix = do
    let skip = []

    let prefixString = Text.unpack prefix

    Test.Util.testCase prefix skip $ do
        let actualPath   = prefixString <> "A.dhall"
        let expectedPath = prefixString <> "B.dhall"

        actualCode   <- Text.IO.readFile actualPath
        expectedCode <- Text.IO.readFile expectedPath

        actualExpr <- throws (Parser.exprFromText mempty actualCode)

        actualResolved <- Import.assertNoImports actualExpr

        let actualNormalized =
                Core.alphaNormalize
                    (Core.normalize actualResolved :: Expr Void Void)

        expectedExpr <- throws (Parser.exprFromText mempty expectedCode)

        expectedResolved <- Import.assertNoImports expectedExpr

        let expectedNormalized =
                Core.alphaNormalize (Core.denote expectedResolved)

        let message =
                "The normalized expression did not match the expected output"

        Tasty.HUnit.assertEqual message expectedNormalized actualNormalized

betaNormalizationTest :: Text -> TestTree
betaNormalizationTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let actualCode   = Test.Util.toDhallPath (prefix <> "A.dhall")
        let expectedCode = Test.Util.toDhallPath (prefix <> "B.dhall")

        actualExpr <- throws (Parser.exprFromText mempty actualCode)

        actualResolved <- Import.load actualExpr

        _ <- throws (TypeCheck.typeOf actualResolved)

        let actualNormalized =
                Core.alphaNormalize
                    (Core.normalize actualResolved :: Expr Void Void)

        expectedExpr <- throws (Parser.exprFromText mempty expectedCode)

        expectedResolved <- Import.load expectedExpr

        _ <- throws (TypeCheck.typeOf expectedResolved)

        -- Use `denote` instead of `normalize` to enforce that the expected
        -- expression is already in normal form
        let expectedNormalized =
                Core.alphaNormalize (Core.denote expectedResolved)

        let message =
                "The normalized expression did not match the expected output"

        Tasty.HUnit.assertEqual message expectedNormalized actualNormalized
