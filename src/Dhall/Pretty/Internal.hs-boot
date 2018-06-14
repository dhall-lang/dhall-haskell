module Dhall.Pretty.Internal where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty, Doc)

import {-# SOURCE #-} Dhall.Core

data Ann

prettyVar :: Var -> Doc Ann

prettyConst :: Const -> Doc Ann

prettyExpr :: Pretty a => Expr s a -> Doc Ann

pretty :: Pretty a => a -> Text

escapeText :: Text -> Text
