{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Import where

import Data.Text (Text)
import Test.Tasty (TestTree)
import Dhall.Import (MissingImports(..))
import Control.Exception (catch, throwIO)
import Data.Monoid ((<>))

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall.Parser
import qualified Dhall.Import
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

tests :: TestTree
tests =
    Test.Tasty.testGroup "import tests"
        [ Test.Tasty.testGroup "import alternatives"
            [ shouldFail
                3
                "alternative of several unset env variables"
                "./tests/import/failure/alternativeEnv.dhall"
            , shouldFail
                1
                "alternative of env variable and missing"
                "./tests/import/failure/alternativeEnvMissing.dhall"
            , shouldFail
                0
                "just missing"
                "./tests/import/failure/missing.dhall"
            , shouldNotFail
                "alternative of env variable, missing, and a Natural"
                "./tests/import/success/alternativeEnvNaturalA.dhall"
            , shouldNotFail
                "alternative of env variable and a Natural"
                "./tests/import/success/alternativeEnvSimpleA.dhall"
            , shouldNotFail
                "alternative of a Natural and missing"
                "./tests/import/success/alternativeNaturalA.dhall"
            ]
        , Test.Tasty.testGroup "import relative to argument"
            [ shouldNotFailRelative
                "a semantic integrity check if fields are reordered"
                "./tests/import/success/"
                "./tests/import/success/fieldOrderA.dhall"
            , shouldNotFailRelative
                "a semantic integrity check when importing an expression using `constructors`"
                "./tests/import/success/"
                "./tests/import/success/issue553B.dhall"
            ]
        ]

shouldNotFail :: Text -> FilePath -> TestTree
shouldNotFail name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    text <- Data.Text.IO.readFile path
    actualExpr <- case Dhall.Parser.exprFromText mempty text of
                     Left  err  -> throwIO err
                     Right expr -> return expr
    _ <- Dhall.Import.load actualExpr
    return ())

shouldNotFailRelative :: Text -> FilePath -> FilePath -> TestTree
shouldNotFailRelative name dir path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    text <- Data.Text.IO.readFile path
    expr <- case Dhall.Parser.exprFromText mempty text of
                     Left  err  -> throwIO err
                     Right expr -> return expr

    _ <- State.evalStateT (Dhall.Import.loadWith expr) (Dhall.Import.emptyStatus dir)

    return ())

shouldFail :: Int -> Text -> FilePath -> TestTree
shouldFail failures name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    text <- Data.Text.IO.readFile path
    actualExpr <- case Dhall.Parser.exprFromText mempty text of
                     Left  err  -> throwIO err
                     Right expr -> return expr
    catch
      (do
          _ <- Dhall.Import.load actualExpr
          fail "Import should have failed, but it succeeds")
      (\(MissingImports es) -> case length es == failures of
                                True -> pure ()
                                False -> fail ("Should have failed "
                                               <> show failures
                                               <> " times, but failed with: \n"
                                               <> show es)) )
