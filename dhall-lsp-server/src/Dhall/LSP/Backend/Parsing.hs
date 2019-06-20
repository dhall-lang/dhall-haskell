module Dhall.LSP.Backend.Parsing (getLetInner, getLetAnnot) where

import Dhall.Src (Src(..))
import Dhall.Parser
import Dhall.Parser.Token
import Dhall.Parser.Expression

import Control.Applicative (optional)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec (SourcePos(..))


-- | Parse the outermost binding in a Src descriptor of a let-block and return
--   the rest. Ex. on input `let a = 0 let b = a in b` parses `let a = 0 ` and
--   returns the Src descriptor containing `let b = a in b`.
getLetInner :: Src -> Maybe Src
getLetInner (Src left _ text) = Megaparsec.parseMaybe (unParser parseLetInnerOffset) text
 where parseLetInnerOffset = do
          setSourcePos left
          _let
          _ <- label
          _ <- optional (do
            _ <- _colon
            expr)
          _equal
          _ <- expr
          _ <- optional _in
          begin <- getSourcePos
          tokens <- Megaparsec.takeRest
          end <- getSourcePos
          return (Src begin end tokens)

-- | Given an Src of a let expression return the Src containing the type
--   annotation. If the let expression does not have a type annotation return
--   a 0-length Src where we can insert one.
getLetAnnot :: Src -> Maybe Src
getLetAnnot (Src left _ text) = Megaparsec.parseMaybe (unParser parseLetAnnot) text
  where parseLetAnnot = do
          setSourcePos left
          _let
          _ <- label
          begin <- getSourcePos
          (tokens, _) <- Megaparsec.match $ optional (do
            _ <- _colon
            expr)
          end <- getSourcePos
          _ <- Megaparsec.takeRest
          return (Src begin end tokens)

setSourcePos :: SourcePos -> Parser ()
setSourcePos src = Megaparsec.updateParserState
                     (\(Megaparsec.State s o (Megaparsec.PosState i o' _ t l)) ->
                       Megaparsec.State s o (Megaparsec.PosState i o' src t l))
