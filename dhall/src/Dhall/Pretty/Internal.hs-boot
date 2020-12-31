module Dhall.Pretty.Internal where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty, Doc)
import Dhall.Src (Src)
import Language.Haskell.TH.Syntax (Lift)

import {-# SOURCE #-} Dhall.Syntax

data Ann

data CharacterSet = ASCII | Unicode

instance Eq CharacterSet
instance Ord CharacterSet
instance Show CharacterSet
instance Data CharacterSet
instance Lift CharacterSet
instance NFData CharacterSet
instance Semigroup CharacterSet
instance Monoid CharacterSet

prettyVar :: Var -> Doc Ann

prettyConst :: Const -> Doc Ann

prettyExpr :: Pretty a => Expr s a -> Doc Ann

prettyEnvironmentVariable :: Text -> Doc ann

prettyImportExpression :: Pretty a => Expr Src a -> Doc Ann
