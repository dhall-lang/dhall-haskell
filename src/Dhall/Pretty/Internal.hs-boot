module Dhall.Pretty.Internal where

import Data.Scientific (Scientific)
import Data.Text.Lazy (Text)
import Data.Text.Prettyprint.Doc (Pretty, Doc)
import Formatting.Buildable (Buildable)
import Numeric.Natural (Natural)
import Prelude

import {-# SOURCE #-} Dhall.Core

data Ann

buildConst :: Const -> Text

buildVar :: Var -> Text

buildExpr :: Buildable a => Expr s a -> Text

prettyExpr :: Pretty a => Expr s a -> Doc Ann

buildNatural :: Natural -> Text

buildNumber :: Integer -> Text

buildScientific :: Scientific -> Text

pretty :: Pretty a => a -> Text

escapeText :: Text -> Text
