module Dhall.LSP.Backend.Formatting (formatExpr, formatExprWithHeader) where

import Dhall.Core (Expr)
import Dhall.Pretty (CharacterSet(..))
import Dhall.Parser (Header(..))
import qualified Dhall.Pretty
import Dhall.Src (Src)

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty

-- | Pretty-print the given Dhall expression.
formatExpr :: Pretty.Pretty b => CharacterSet -> Expr Src b -> Text
formatExpr charSet expr =
      Pretty.renderStrict
    . Dhall.Pretty.layout
    $ Dhall.Pretty.prettyCharacterSet charSet expr

-- | Pretty-print the given Dhall expression, prepending the given a "header"
--   (usually consisting of comments and whitespace).
formatExprWithHeader :: Pretty.Pretty b => CharacterSet -> Expr Src b -> Header -> Text
formatExprWithHeader charSet expr (Header header) = Pretty.renderStrict
  (Dhall.Pretty.layout doc)
  where
    doc =
      Pretty.pretty header
        <> Dhall.Pretty.prettyCharacterSet charSet expr
        <> "\n"

