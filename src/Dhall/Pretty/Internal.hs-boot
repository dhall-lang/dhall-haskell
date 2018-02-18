module Dhall.Pretty.Internal where

import Data.Scientific (Scientific)
import Data.Text.Buildable (Buildable)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Prettyprint.Doc (Pretty, Doc)
import Numeric.Natural (Natural)
import Prelude

import {-# SOURCE #-} Dhall.Core

data Ann

buildConst :: Const -> Builder

buildVar :: Var -> Builder

buildExpr :: Buildable a => Expr s a -> Builder

prettyExpr :: Pretty a => Expr s a -> Doc Ann

buildNatural :: Natural -> Builder

buildNumber :: Integer -> Builder

buildScientific :: Scientific -> Builder

pretty :: Pretty a => a -> Text

escapeText :: Builder -> Builder
