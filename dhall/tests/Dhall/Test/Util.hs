{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Dhall.Test.Util
    (
      code
    ) where

-- import qualified Control.Exception
-- import qualified Data.Functor
-- import           Data.Bifunctor (first)
-- import           Data.Text (Text)
-- import qualified Dhall.Core
-- import           Dhall.Core (Expr)
-- import qualified Dhall.Context
-- import           Dhall.Context (Cxt)
-- import qualified Dhall.Import

import Control.Exception
import Dhall.Elaboration
import Dhall.Eval
import Dhall.Parser
import Test.Tasty.HUnit
import Data.Text

code :: Text -> IO Core
code src = do
  expr <- case Dhall.Parser.exprFromText mempty src of
    Left parseError -> Control.Exception.throwIO parseError
    Right expr      -> pure expr
  fst <$> infer0 "." expr

-- equivalent :: Text -> Text -> IO ()
-- equivalent text0 text1 = do
--     expr0 <- fmap Dhall.Core.normalize (code text0) :: IO (Expr X X)
--     expr1 <- fmap Dhall.Core.normalize (code text1) :: IO (Expr X X)
--     assertEqual "Expressions are not equivalent" expr0 expr1

-- assertNormalized :: Expr Src X -> IO ()
-- assertNormalized e = do
--   assertBool msg1 (Dhall.Core.isNormalized e)
--   assertEqual msg2 (normalize' e) (Dhall.Core.pretty e)
--   where msg1 = "Expression was not in normal form"
--         msg2 = "Normalization is not supposed to change the expression"
