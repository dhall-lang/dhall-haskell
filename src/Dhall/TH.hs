{-# LANGUAGE TemplateHaskell #-}

module Dhall.TH where

import           Control.Monad
import qualified Data.Text.Lazy as Text
import           Data.Typeable
import qualified Dhall.Core as D
import qualified Dhall.Import as D
import qualified Dhall.Parser as D
import qualified Dhall.TypeCheck as D
import           Language.Haskell.TH.Syntax

-- A workaround for a problem in TemplateHaskell (see
-- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable)
liftText :: Text.Text -> Q Exp
liftText = fmap (AppE (VarE 'Text.pack)) . lift . Text.unpack

importDhallExpression :: Text.Text -> IO (D.Expr D.Src D.X)
importDhallExpression =
  either (const (fail "invalid Dhall expression")) D.load
  . D.exprFromText "(static)"

-- | This resolves all imports in the expression, so the resulting AST is self-
--   contained.
staticDhallExpression :: Text.Text -> Q Exp
staticDhallExpression =
  dataToExpQ (\a -> liftText <$> cast a) <=< runIO . importDhallExpression
