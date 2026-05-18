{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Normalization where

import Control.Monad.Trans.State.Strict (StateT)
import Data.Text       (Text)
import Data.Void       (Void)
import Dhall.Core      (Expr (..), Import (..), ImportHashed (..), ImportMode (..), ImportType (..), File (..), Directory (..), FilePrefix (..), Var (..), throws)
import Dhall.Import    (Status)
import System.FilePath ((</>))
import Test.Tasty      (TestTree)

import qualified Control.Exception                as Control.Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text.IO
import qualified Dhall.Context                    as Context
import qualified Dhall.Core                       as Core
import qualified Dhall.Import                     as Import
import qualified Dhall.Parser                     as Parser
import qualified Dhall.Test.Util                  as Test.Util
import qualified Dhall.TypeCheck                  as TypeCheck
import qualified System.FilePath                  as FilePath
import qualified Test.Tasty                       as Tasty
import qualified Test.Tasty.HUnit                 as Tasty.HUnit
import qualified Turtle

normalizationDirectory :: FilePath
normalizationDirectory = "./dhall-lang/tests/normalization/success"

unitDirectory :: FilePath
unitDirectory = normalizationDirectory </> "unit/"

getTests :: IO TestTree
getTests = do
    let pattern = Turtle.chars <* "A.dhall"

    let normalizationFiles = do
            path <- FilePath.normalise <$> Turtle.lstree normalizationDirectory

            unitDirectory `Test.Util.pathNotPrefixOf` path

            return path

    betaNormalizationTests <- Test.Util.discover pattern betaNormalizationTest normalizationFiles

    alphaNormalizationTests <-
        Test.Util.discover pattern alphaNormalizationTest
            (Turtle.lstree "./dhall-lang/tests/alpha-normalization/success/")

    let unitTestFiles = Turtle.lstree unitDirectory

    unitTests <- Test.Util.discover pattern unitTest unitTestFiles

    let testTree =
            Tasty.testGroup "normalization"
                [ Tasty.testGroup "beta-normalization"
                    [ betaNormalizationTests
                    ]
                , Tasty.testGroup "unit tests"
                    [ unitTests
                    ]
                , Tasty.testGroup "alpha-normalization"
                    [ alphaNormalizationTests
                    ]
                , proposalTests
                , customization
                ]

    return testTree

proposalTests :: TestTree
proposalTests =
    Tasty.testGroup "proposals"
        [ outerLetPreservation
        , sha256ImportPreservation
        ]

outerLetPreservation :: TestTree
outerLetPreservation =
    Tasty.testGroup "outer let preservation"
        [ Tasty.HUnit.testCase "preserved inside lambda" $ do
            e <- Test.Util.code "let a = 1 in λ(x : Natural) → a + x"
            let normalized = Core.normalize e
            Tasty.HUnit.assertEqual "normal form"
                (Core.pretty normalized)
                "let a = 1 in λ(x : Natural) → a + x"
        , Tasty.HUnit.testCase "inlined when not inside lambda" $ do
            e <- Test.Util.code "let a = 1 in a + 2"
            let normalized = Core.normalize e
            Tasty.HUnit.assertEqual "normal form"
                (Core.pretty normalized)
                "3"
        , Tasty.HUnit.testCase "inner let inlined inside lambda" $ do
            e <- Test.Util.code "λ(x : Natural) → let a = 1 in a + x"
            let normalized = Core.normalize e
            Tasty.HUnit.assertEqual "normal form"
                (Core.pretty normalized)
                "λ(x : Natural) → 1 + x"
        , Tasty.HUnit.testCase "outer let inlined after beta reduction" $ do
            e <- Test.Util.code "let a = 1 in (λ(x : Natural) → a + x) 2"
            let normalized = Core.normalize e
            Tasty.HUnit.assertEqual "normal form"
                (Core.pretty normalized)
                "3"
        , Tasty.HUnit.testCase "nested lambdas preserve outer lets" $ do
            e <- Test.Util.code "let a = 1 in λ(x : Natural) → λ(y : Natural) → a + x + y"
            let normalized = Core.normalize e
            Tasty.HUnit.assertEqual "normal form"
                (Core.pretty normalized)
                "let a = 1 in λ(x : Natural) → λ(y : Natural) → a + x + y"
        ]

sha256ImportPreservation :: TestTree
sha256ImportPreservation =
    Tasty.HUnit.testCase "sha256 import preserved in normal form" $ do
        let importContent = "1\n"
        -- Create a temporary file to import
        Turtle.with (Turtle.mktempfile "." "test.dhall") $ \tmpFile -> do
            Turtle.writeTextFile tmpFile importContent

            -- Parse and resolve the file to compute its semantic hash
            parsed <- case Parser.exprFromText mempty importContent of
                Left parseError -> Control.Exception.throwIO parseError
                Right expr0     -> return expr0
            resolved <- Import.load parsed
            let semExpr = Core.alphaNormalize (Core.normalize resolved :: Expr Void Void)
            let hash = Import.hashExpression semExpr

            -- Build an import expression with the hash
            let importExpr =
                    Embed
                        ( Import
                            { importHashed = ImportHashed
                                { hash = Just hash
                                , importType = Local Here
                                    ( File
                                        { directory = Directory []
                                        , file = Turtle.format Turtle.fp (Turtle.filename tmpFile)
                                        }
                                    )
                                }
                            , importMode = Code
                            }
                        )

            -- Load preserving the import
            loaded <- State.evalStateT
                (Import.loadWithPreserving importExpr)
                (Import.emptyStatus ".")

            -- Normalize
            let normalized = Core.normalize loaded

            -- The import should still be present in the AST
            case normalized of
                Embed preservedImport -> do
                    let expectedHash = hash
                    let actualHash = Core.hash (Core.importHashed preservedImport)
                    Tasty.HUnit.assertEqual "Hash preserved" (Just expectedHash) actualHash
                _ -> do
                    fail ("Expected import to be preserved, but got: " <> Text.unpack (Core.pretty normalized))

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
                (Pi mempty "_" Natural (Pi mempty "_" Natural Natural))
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
                "The alpha-normalized expression did not match the expected output"

        Tasty.HUnit.assertEqual message expectedNormalized actualNormalized

{- Unit tests don't type-check, so we only verify that they normalize to the
   expected output
-}
unitTest :: Text -> TestTree
unitTest prefix = do
    let expectedFailures = []

    let prefixString = Text.unpack prefix

    Test.Util.testCase prefix expectedFailures $ do
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
betaNormalizationTest prefix = do
    let prefixString = Text.unpack prefix

    Tasty.HUnit.testCase prefixString $ do
        let actualCode = Test.Util.toDhallPath (prefix <> "A.dhall")

        let expectedPath = prefixString <> "B.dhall"
        expectedCode <- Text.IO.readFile expectedPath

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
                "The beta-normalized expression did not match the expected output"

        Tasty.HUnit.assertEqual message expectedNormalized actualNormalized
