module Dhall.LSP.Backend.Formatting (formatExpr, formatExprWithHeader) where

import Data.Maybe   (fromMaybe)
import Data.Text    (Text)
import Dhall.Core   (Expr)
import Dhall.Parser (Header (..))
import Dhall.Pretty (CharacterSet (..))
import Dhall.Src    (Src)

import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall.Pretty

-- | Pretty-print the given Dhall expression.
formatExpr :: Pretty.Pretty b => Maybe CharacterSet -> Expr Src b -> Text
formatExpr chosenCharacterSet expr =
      Pretty.renderStrict
    . Dhall.Pretty.layout
    $ Dhall.Pretty.prettyCharacterSet charSet expr
  where
    charSet = fromMaybe (Dhall.Pretty.detectCharacterSet expr) chosenCharacterSet

-- | Pretty-print the given Dhall expression, prepending the given a "header"
--   (usually consisting of comments and whitespace).
formatExprWithHeader :: Pretty.Pretty b => Maybe CharacterSet -> Expr Src b -> Header -> Text
formatExprWithHeader chosenCharacterSet expr (Header header) = Pretty.renderStrict
  (Dhall.Pretty.layout doc)
  where
    charSet = fromMaybe (Dhall.Pretty.detectCharacterSet expr) chosenCharacterSet

    doc =
      Pretty.pretty header
        <> Dhall.Pretty.prettyCharacterSet charSet expr
        <> "\n"
