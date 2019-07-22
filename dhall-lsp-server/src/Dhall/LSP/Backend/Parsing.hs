module Dhall.LSP.Backend.Parsing
  ( getImportHash
  , getLetInner
  , getLetAnnot
  , getLetIdentifier
  , getLamIdentifier
  , getForallIdentifier)
where

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

-- | Given an Src of a let expression return the Src containing the bound
--   identifier, i.e. given `let x = ... in ...` return the Src descriptor
--   containing `x`. Returns the original Src if something goes wrong.
getLetIdentifier :: Src -> Src
getLetIdentifier src@(Src left _ text) =
  case Megaparsec.parseMaybe (unParser parseLetIdentifier) text of
    Just src' -> src'
    Nothing -> src
  where parseLetIdentifier = do
          setSourcePos left
          _let
          begin <- getSourcePos
          (tokens, _) <- Megaparsec.match label
          end <- getSourcePos
          _ <- Megaparsec.takeRest
          return (Src begin end tokens)

-- | Cf. `getLetIdentifier`.
getLamIdentifier :: Src -> Src
getLamIdentifier src@(Src left _ text) =
  case Megaparsec.parseMaybe (unParser parseLetIdentifier) text of
    Just src' -> src'
    Nothing -> src
  where parseLetIdentifier = do
          setSourcePos left
          _lambda
          _openParens
          begin <- getSourcePos
          (tokens, _) <- Megaparsec.match label
          end <- getSourcePos
          _ <- Megaparsec.takeRest
          return (Src begin end tokens)

-- | Cf. `getLetIdentifier`.
getForallIdentifier :: Src -> Src
getForallIdentifier src@(Src left _ text) =
  case Megaparsec.parseMaybe (unParser parseLetIdentifier) text of
    Just src' -> src'
    Nothing -> src
  where parseLetIdentifier = do
          setSourcePos left
          _forall
          _openParens
          begin <- getSourcePos
          (tokens, _) <- Megaparsec.match label
          end <- getSourcePos
          _ <- Megaparsec.takeRest
          return (Src begin end tokens)

-- | Given an Src of a import expression return the Src containing the hash
--   annotation. If the import does not have a hash annotation return a 0-length
--   Src where we can insert one.
getImportHash :: Src -> Maybe Src
getImportHash (Src left _ text) =
  Megaparsec.parseMaybe (unParser parseImportHashPosition) text
  where parseImportHashPosition = do
          setSourcePos left
          _ <- importType_
          begin <- getSourcePos
          (tokens, _) <- Megaparsec.match $ optional importHash_
          end <- getSourcePos
          _ <- Megaparsec.takeRest
          return (Src begin end tokens)

setSourcePos :: SourcePos -> Parser ()
setSourcePos src = Megaparsec.updateParserState
                     (\(Megaparsec.State s o (Megaparsec.PosState i o' _ t l)) ->
                       Megaparsec.State s o (Megaparsec.PosState i o' src t l))
