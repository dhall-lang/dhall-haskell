module Dhall.LSP.Backend.Formatting (formatDocument, formatExpr) where

import Dhall.Core (Expr)
import Dhall.Pretty (CharacterSet(..), layoutOpts, prettyCharacterSet)
import Dhall.Parser(exprAndHeaderFromText, ParseError(..))

import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty

formatDocument :: Text -> Either ParseError Text
formatDocument text = do
  (header, expr) <- exprAndHeaderFromText "" text
  pure (formatExpr header expr)

formatExpr :: Pretty.Pretty b => Text -> Expr a b -> Text
formatExpr header expr = Pretty.renderStrict
  (Pretty.layoutSmart layoutOpts doc)
  where
    doc =
      Pretty.pretty header
        <> Pretty.unAnnotate (prettyCharacterSet Unicode expr)
        <> "\n"
