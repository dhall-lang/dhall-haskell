module Dhall.LSP.Backend.Formatting (formatExpr, formatExprWithHeader) where

import Dhall.Core (Expr)
import Dhall.Pretty (CharacterSet(..), layoutOpts, prettyCharacterSet)

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty

-- | Pretty-print the given Dhall expression.
formatExpr :: Pretty.Pretty b => Expr a b -> Text
formatExpr expr = formatExprWithHeader expr ""

-- | Pretty-print the given Dhall expression, prepending the given a "header"
--   (usually consisting of comments and whitespace).
formatExprWithHeader :: Pretty.Pretty b => Expr a b -> Text -> Text
formatExprWithHeader expr header = Pretty.renderStrict
  (Pretty.layoutSmart layoutOpts doc)
  where
    doc =
      Pretty.pretty header
        <> Pretty.unAnnotate (prettyCharacterSet Unicode expr)
        <> "\n"

