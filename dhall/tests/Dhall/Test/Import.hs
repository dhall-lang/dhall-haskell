{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Import where

import Data.Text (Text)
import Test.Tasty (TestTree)
-- import Dhall.Import (MissingImports(..))
-- import Dhall.Parser (SourcedException(..))
import Control.Exception (catch, throwIO)
-- import Data.Monoid ((<>))

import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall.Parser
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

import Control.Monad.Reader
import Dhall.Context
import Dhall.Elaboration

tests :: TestTree
tests =
    Test.Tasty.testGroup "import tests"
        [ Test.Tasty.testGroup "import alternatives"
            [ shouldFail
                3
                "alternative of several unset env variables"
                "./dhall-lang/tests/import/failure/alternativeEnv.dhall"
            , shouldFail
                1
                "alternative of env variable and missing"
                "./dhall-lang/tests/import/failure/alternativeEnvMissing.dhall"
            , shouldFail
                0
                "just missing"
                "./dhall-lang/tests/import/failure/missing.dhall"
            , shouldNotFail
                "alternative of env variable, missing, and a Natural"
                "./dhall-lang/tests/import/success/alternativeEnvNaturalA.dhall"
            , shouldNotFail
                "alternative of env variable and a Natural"
                "./dhall-lang/tests/import/success/alternativeEnvSimpleA.dhall"
            , shouldNotFail
                "alternative of a Natural and missing"
                "./dhall-lang/tests/import/success/alternativeNaturalA.dhall"
            ]
        , Test.Tasty.testGroup "import relative to argument"
            [ shouldNotFailRelative
                "a semantic integrity check if fields are reordered"
                "./dhall-lang/tests/import/success/"
                "./dhall-lang/tests/import/success/fieldOrderA.dhall"
            ]
        ]

shouldNotFail :: Text -> FilePath -> TestTree
shouldNotFail name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
    text <- Data.Text.IO.readFile path
    actualExpr <- case Dhall.Parser.exprFromText mempty text of
                     Left  err  -> throwIO err
                     Right expr -> return expr
    _ <- infer0 "." actualExpr
    return ()

shouldNotFailRelative :: Text -> FilePath -> FilePath -> TestTree
shouldNotFailRelative name dir path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
    text <- Data.Text.IO.readFile path
    expr <- case Dhall.Parser.exprFromText mempty text of
                     Left  err  -> throwIO err
                     Right expr -> return expr
    _ <- runReaderT (infer emptyCxt expr) =<< emptyImportState dir
    return ()

shouldFail :: Int -> Text -> FilePath -> TestTree
shouldFail _ name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
    text <- Data.Text.IO.readFile path
    actualExpr <- case Dhall.Parser.exprFromText mempty text of
                     Left  err  -> throwIO err
                     Right expr -> return expr
    catch
      (do
          _ <- infer0 "." actualExpr
          fail "Import should have failed, but it succeeds")
      (\(ImportError _) -> pure ()
          -- case length es == failures of
          --     True -> pure ()
          --     False -> fail
          --         (   "Should have failed "
          --         <>  show failures
          --         <>  " times, but failed with: \n"
          --         <>  show es
          --         )
      )
