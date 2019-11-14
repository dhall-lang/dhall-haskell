module Dhall.LSP.Backend.Parsing
  ( getImportLink
  , getImportHash
  , getLetInner
  , getLetAnnot
  , getLetIdentifier
  , getLamIdentifier
  , getForallIdentifier
  , binderExprFromText
  , holeExpr
  )
where

import Data.Text (Text)
import Dhall.Core (Binding(..), Expr(..), Import, Var(..))
import Dhall.Src (Src(..))
import Dhall.Parser
import Dhall.Parser.Token hiding (text)
import Dhall.Parser.Expression (getSourcePos, importType_, importHash_, localOnly)
import Text.Megaparsec (try, skipManyTill, lookAhead, anySingle,
  notFollowedBy, eof, takeRest)

import Control.Applicative (optional, (<|>))
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
          nonemptyWhitespace
          _ <- label
          whitespace
          _ <- optional (do
            _ <- _colon
            nonemptyWhitespace
            _ <- expr
            whitespace)
          _equal
          whitespace
          _ <- expr
          whitespace
          _ <- optional _in
          whitespace
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
          nonemptyWhitespace
          _ <- label
          whitespace
          begin <- getSourcePos
          (tokens, _) <- Megaparsec.match $ optional (do
            _ <- _colon
            nonemptyWhitespace
            _ <- expr
            whitespace)
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
          nonemptyWhitespace
          begin <- getSourcePos
          (tokens, _) <- Megaparsec.match label
          end <- getSourcePos
          _ <- Megaparsec.takeRest
          return (Src begin end tokens)

-- | Cf. `getLetIdentifier`.
getLamIdentifier :: Src -> Maybe Src
getLamIdentifier (Src left _ text) =
  Megaparsec.parseMaybe (unParser parseLetIdentifier) text
  where parseLetIdentifier = do
          setSourcePos left
          _lambda
          whitespace
          _openParens
          whitespace
          begin <- getSourcePos
          (tokens, _) <- Megaparsec.match label
          end <- getSourcePos
          _ <- Megaparsec.takeRest
          return (Src begin end tokens)

-- | Cf. `getLetIdentifier`.
getForallIdentifier :: Src -> Maybe Src
getForallIdentifier (Src left _ text) =
  Megaparsec.parseMaybe (unParser parseLetIdentifier) text
  where parseLetIdentifier = do
          setSourcePos left
          _forall
          whitespace
          _openParens
          whitespace
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
          whitespace
          begin <- getSourcePos
          (tokens, _) <- Megaparsec.match $ optional importHash_
          end <- getSourcePos
          _ <- Megaparsec.takeRest
          return (Src begin end tokens)

setSourcePos :: SourcePos -> Parser ()
setSourcePos src = Megaparsec.updateParserState
                     (\(Megaparsec.State s o (Megaparsec.PosState i o' _ t l)) ->
                       Megaparsec.State s o (Megaparsec.PosState i o' src t l))

getImportLink :: Src -> Src
getImportLink src@(Src left _ text) =
  case Megaparsec.parseMaybe (unParser parseImportLink) text of
    Just src' -> src'
    Nothing -> src
 where
  parseImportLink = do
    setSourcePos left
    begin <- getSourcePos
    (tokens, _) <-
      Megaparsec.match $ (localOnly *> return ()) <|> (httpRaw *> return ())
    end <- getSourcePos
    _ <- Megaparsec.takeRest
    return (Src begin end tokens)

-- | An expression that is guaranteed not to typecheck. Can be used a
-- placeholder type to emulate 'lazy' contexts, when typechecking something in a
-- only partly well-typed context.
holeExpr :: Expr s a
-- The illegal variable name ensures that it can't be bound by the user!
holeExpr = Var (V "" 0)

-- | Approximate the type-checking context at the end of the input. Tries to
-- parse as many binders as possible. Very messy!
binderExprFromText :: Text -> Expr Src Import
binderExprFromText txt =
  case Megaparsec.parseMaybe (unParser parseBinderExpr) (txt <> " ") of
    Just e -> e
    Nothing -> holeExpr
  where
    -- marks the beginning of the next binder
    boundary = _let <|> _forall <|> _lambda

    -- A binder that is out of scope at the end of the input. Discarded in the
    -- resulting 'binder expression'.
    closedBinder = closedLet <|> closedLambda <|> closedPi

    closedLet = do
      _let
      nonemptyWhitespace
      _ <- label
      whitespace
      _ <- optional (do
        _colon
        nonemptyWhitespace
        expr)
      _equal
      whitespace
      _ <- expr
      whitespace
      (do
        _in
        nonemptyWhitespace
        _ <- expr
        return ())
        <|> closedLet

    closedLambda = do
      _lambda
      whitespace
      _openParens
      whitespace
      _ <- label
      whitespace
      _colon
      nonemptyWhitespace
      _ <- expr
      whitespace
      _closeParens
      whitespace
      _arrow
      whitespace
      _ <- expr
      return ()

    closedPi = do
      _forall
      whitespace
      _openParens
      whitespace
      _ <- label
      whitespace
      _colon
      nonemptyWhitespace
      _ <- expr
      whitespace
      _closeParens
      whitespace
      _arrow
      whitespace
      _ <- expr
      return ()

    -- Try to parse as many binders as possible. Skip malformed input and
    -- 'closed' binders that are already out of scope at the end of the input.
    parseBinderExpr = do
      try (do
          skipManyTill anySingle (lookAhead boundary)
          try (do
              closedBinder
              notFollowedBy eof
              parseBinderExpr)
            <|> try (letBinder <|> lambdaBinder <|> forallBinder)
            <|> (do
              boundary
              parseBinderExpr))
        <|> (do
          _ <- takeRest
          return holeExpr)

    letBinder = do
      _let
      nonemptyWhitespace
      name <- label
      whitespace
      mType <- optional (do _colon; nonemptyWhitespace; _type <- expr; whitespace; return (Nothing, _type))

      -- if the bound value does not parse, skip and replace with 'hole'
      value <- try (do _equal; whitespace; expr <* whitespace)
          <|> (do skipManyTill anySingle (lookAhead boundary <|> _in); return holeExpr)
      inner <- parseBinderExpr
      return (Let (Binding Nothing name Nothing mType Nothing value) inner)

    forallBinder = do
      _forall
      whitespace
      _openParens
      whitespace
      name <- label
      whitespace
      _colon
      nonemptyWhitespace
      -- if the bound type does not parse, skip and replace with 'hole'
      typ <- try (do e <- expr; whitespace; _closeParens; whitespace; _arrow; return e)
          <|> (do skipManyTill anySingle _arrow; return holeExpr)
      whitespace
      inner <- parseBinderExpr
      return (Pi name typ inner)

    lambdaBinder = do
      _lambda
      whitespace
      _openParens
      whitespace
      name <- label
      whitespace
      _colon
      nonemptyWhitespace
      -- if the bound type does not parse, skip and replace with 'hole'
      typ <- try (do e <- expr; whitespace; _closeParens; whitespace; _arrow; return e)
          <|> (do skipManyTill anySingle _arrow; return holeExpr)
      whitespace
      inner <- parseBinderExpr
      return (Lam name typ inner)
