module Dhall.LSP.Backend.Formatting (formatExpr, formatExprWithHeader) where

import Data.Text    (Text)
import Dhall.Core   (Expr)
import Dhall.Parser (Header (..))
import Dhall.Pretty (CharacterSet (..))
import Dhall.Src    (Src)

import qualified Dhall.Pretty
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty

-- | Pretty-print the given Dhall expression.
formatExpr :: Pretty.Pretty b => ChooseCharacterSet -> Expr Src b -> Text
formatExpr chosenCharacterSet expr =
      Pretty.renderStrict
    . Dhall.Pretty.layout
    $ Dhall.Pretty.prettyCharacterSet charSet expr
  where
    charSet = chooseCharsetOrUseDefault (Dhall.Pretty.detectCharacterSet expr) chosenCharacterSet

-- | Pretty-print the given Dhall expression, prepending the given a "header"
--   (usually consisting of comments and whitespace).
formatExprWithHeader :: Pretty.Pretty b => ChooseCharacterSet -> Expr Src b -> Header -> Text
formatExprWithHeader chosenCharacterSet expr (Header header) = Pretty.renderStrict
  (Dhall.Pretty.layout doc)
  where
    charSet = chooseCharsetOrUseDefault (Dhall.Pretty.detectCharacterSet expr) chosenCharacterSet

    doc =
      Pretty.pretty header
        <> Dhall.Pretty.prettyCharacterSet charSet expr
        <> "\n"
