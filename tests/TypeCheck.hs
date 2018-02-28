{-# LANGUAGE OverloadedStrings #-}

module TypeCheck where

import Data.Monoid (mempty, (<>))
import Data.Text.Lazy (Text)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)
import Test.Tasty (TestTree)

import qualified Control.Exception
import qualified Data.Text.Lazy
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

typecheckTests :: TestTree
typecheckTests =
    Test.Tasty.testGroup "typecheck tests"
        [ should
            "allow type-valued fields in a record"
            "fieldsAreTypes"
        , should
            "allow type-valued alternatives in a union"
            "alternativesAreTypes"
        , should
            "allow anonymous functions in types to be judgmentally equal"
            "anonymousFunctionsInTypes"
        ]

should :: Text -> Text -> TestTree
should name basename =
    Test.Tasty.HUnit.testCase (Data.Text.Lazy.unpack name) $ do
        let actualCode   = "./tests/typecheck/" <> basename <> "A.dhall"
        let expectedCode = "./tests/typecheck/" <> basename <> "B.dhall"

        actualExpr <- case Dhall.Parser.exprFromText mempty actualCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        actualResolved <- Dhall.Import.load actualExpr
        actualType <- case Dhall.TypeCheck.typeOf actualResolved of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        let actualNormalized = Dhall.Core.normalize actualType :: Expr X X

        expectedExpr <- case Dhall.Parser.exprFromText mempty expectedCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        expectedResolved <- Dhall.Import.load expectedExpr
        let expectedNormalized = Dhall.Core.normalize expectedResolved

        let message = "The expression's type did not match the expected type"
        Test.Tasty.HUnit.assertEqual message expectedNormalized actualNormalized
