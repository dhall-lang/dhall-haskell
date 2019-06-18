module Dhall.LSP.Backend.Parsing (getLetInner) where

import Dhall.Src (Src(..))
import Dhall.Parser
-- import Dhall.Parser.Combinators
import Dhall.Parser.Token
import Dhall.Parser.Expression

import Control.Applicative (optional)
import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec (SourcePos(..))

parseLetInnerOffset :: Parser (Int, SourcePos)
parseLetInnerOffset = do
  _let
  _ <- label
  _ <- optional (do
    _ <- _colon
    expr)
  _equal
  _ <- expr
  _ <- optional _in
  off <- getOffset
  pos <- getSourcePos
  _ <- Megaparsec.takeRest
  return (off, pos)

-- | Parse the outermost binding in a Src descriptor of a let-block and return
--   the rest. Ex. on input `let a = 0 let b = a in b` parses `let a = 0 ` and
--   returns the Src descriptor containing `let b = a in b`.
getLetInner :: Src -> Maybe Src
getLetInner (Src left right text) =
  case Megaparsec.parseMaybe (unParser parseLetInnerOffset) text of
    Just (n, dpos) -> Just $ Src (addPosDelta left dpos) right (Text.drop n text)
    Nothing -> Nothing

addPosDelta :: SourcePos -> SourcePos -> SourcePos
addPosDelta (SourcePos name line col) (SourcePos _ dline dcol) =
  (SourcePos name
    (Megaparsec.mkPos $ Megaparsec.unPos line + Megaparsec.unPos dline - 1)
    (Megaparsec.mkPos $ Megaparsec.unPos col + Megaparsec.unPos dcol - 1))
