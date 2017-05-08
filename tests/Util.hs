{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Util (code, normalize', assertNormalizesTo, assertNormalized) where

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Dhall.Core
import           Dhall.Core (Expr)
import qualified Dhall.Import
import qualified Dhall.Parser
import           Dhall.Parser (Src)
import qualified Dhall.TypeCheck
import           Dhall.TypeCheck (X)
import           Test.Tasty.HUnit

normalize' :: Expr Src X -> Data.Text.Lazy.Text
normalize' = Dhall.Core.pretty . Dhall.Core.normalize

code :: Data.Text.Text -> IO (Expr Src X)
code strictText = do
    let lazyText = Data.Text.Lazy.fromStrict strictText
    expr0 <- case Dhall.Parser.exprFromText mempty lazyText of
        Left parseError -> Control.Exception.throwIO parseError
        Right expr0     -> return expr0
    expr1 <- Dhall.Import.load expr0
    case Dhall.TypeCheck.typeOf expr1 of
        Left typeError -> Control.Exception.throwIO typeError
        Right _        -> return ()
    return expr1

assertNormalizesTo :: Expr Src X -> Data.Text.Lazy.Text -> IO ()
assertNormalizesTo e expected = do
  assertBool msg (not $ Dhall.Core.isNormalized e)
  normalize' e @?= expected
  where msg = "Given expression is already in normal form"

assertNormalized :: Expr Src X -> IO ()
assertNormalized e = do
  assertBool msg1 (Dhall.Core.isNormalized e)
  assertEqual msg2 (normalize' e) (Dhall.Core.pretty e)
  where msg1 = "Expression was not in normal form"
        msg2 = "Normalization is not supposed to change the expression"
