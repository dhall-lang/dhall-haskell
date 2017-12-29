{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Util
    ( code
    , codeWith
    , equivalent
    , normalize'
    , normalizeWith'
    , assertNormalizesTo
    , assertNormalizesToWith
    , assertNormalized
    , assertTypeChecks
    ) where

import qualified Control.Exception
import qualified Data.Functor
import           Data.Bifunctor (first)
import           Data.Text (Text)
import qualified Data.Text.Lazy
import qualified Dhall.Core
import           Dhall.Core (Expr, Normalizer)
import qualified Dhall.Context
import           Dhall.Context (Context)
import qualified Dhall.Import
import qualified Dhall.Parser
import           Dhall.Parser (Src)
import qualified Dhall.TypeCheck
import           Dhall.TypeCheck (X)
import           Test.Tasty.HUnit

normalize' :: Expr Src X -> Data.Text.Lazy.Text
normalize' = Dhall.Core.pretty . Dhall.Core.normalize

normalizeWith' :: Normalizer X -> Expr Src X -> Data.Text.Lazy.Text
normalizeWith' ctx = Dhall.Core.pretty . Dhall.Core.normalizeWith ctx

code :: Data.Text.Text -> IO (Expr Src X)
code = codeWith Dhall.Context.empty

codeWith :: Context (Expr Src X) -> Data.Text.Text -> IO (Expr Src X)
codeWith ctx strictText = do
    let lazyText = Data.Text.Lazy.fromStrict strictText
    expr0 <- case Dhall.Parser.exprFromText mempty lazyText of
        Left parseError -> Control.Exception.throwIO parseError
        Right expr0     -> return expr0
    expr1 <- Dhall.Import.load expr0
    case Dhall.TypeCheck.typeWith ctx expr1 of
        Left typeError -> Control.Exception.throwIO typeError
        Right _        -> return ()
    return expr1

equivalent :: Data.Text.Text -> Data.Text.Text -> IO ()
equivalent text0 text1 = do
    expr0 <- fmap Dhall.Core.normalize (Util.code text0) :: IO (Expr X X)
    expr1 <- fmap Dhall.Core.normalize (Util.code text1) :: IO (Expr X X)
    assertEqual "Expressions are not equivalent" expr0 expr1

assertNormalizesTo :: Expr Src X -> Data.Text.Lazy.Text -> IO ()
assertNormalizesTo e expected = do 
  assertBool msg (not $ Dhall.Core.isNormalized e)
  normalize' e @?= expected
  where msg = "Given expression is already in normal form"

assertNormalizesToWith :: Normalizer X -> Expr Src X -> Data.Text.Lazy.Text -> IO ()
assertNormalizesToWith ctx e expected = do
  assertBool msg (not $ Dhall.Core.isNormalizedWith ctx (first (const ()) e))
  normalizeWith' ctx e @?= expected
  where msg = "Given expression is already in normal form"

assertNormalized :: Expr Src X -> IO ()
assertNormalized e = do
  assertBool msg1 (Dhall.Core.isNormalized e)
  assertEqual msg2 (normalize' e) (Dhall.Core.pretty e)
  where msg1 = "Expression was not in normal form"
        msg2 = "Normalization is not supposed to change the expression"

assertTypeChecks :: Text -> IO ()
assertTypeChecks text = Data.Functor.void (code text)
