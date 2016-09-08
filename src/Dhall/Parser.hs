{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

-- | Parsing logic for the Dhall language

module Dhall.Parser (
    -- * Parser
    exprFromText,

    -- * Errors
    ParseError(..),
    ParseMessage(..)
    ) where

import Control.Applicative hiding (Const)
import Control.Exception (Exception)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (Except, throwE, runExceptT)
import Control.Monad.Trans.State.Strict (evalState, get)
import Data.Monoid
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Typeable (Typeable)
import Filesystem.Path.CurrentOS (FilePath)
import Dhall.Core (Const(..), Path(..), Let(..), Expr(..))
import Dhall.Lexer (LocatedToken(..), Position(..), Token)
import Numeric.Natural (Natural)
import Prelude hiding (FilePath)
import Text.Earley

import qualified Data.Map
import qualified Data.Vector
import qualified Pipes.Prelude  as Pipes
import qualified Data.Text.Lazy as Text
import qualified Dhall.Lexer    as Lexer

match :: Token -> Prod r Token LocatedToken Token
match t = fmap Lexer.token (satisfy predicate) <?> t
  where
    predicate (LocatedToken t' _) = t == t'

label :: Prod r e LocatedToken Text
label = fmap unsafeFromLabel (satisfy isLabel)
  where
    isLabel (LocatedToken (Lexer.Label _) _) = True
    isLabel  _                               = False

    unsafeFromLabel (LocatedToken (Lexer.Label l) _) = l

bool :: Prod r e LocatedToken Bool
bool = fmap unsafeFromBool (satisfy isBool)
  where
    isBool (LocatedToken (Lexer.BoolLit _) _) = True
    isBool  _                                 = False

    unsafeFromBool (LocatedToken (Lexer.BoolLit b) _) = b

number :: Prod r e LocatedToken Natural
number = fmap unsafeFromNumber (satisfy isNumber)
  where
    isNumber (LocatedToken (Lexer.Number _) _) = True
    isNumber  _                                = False

    unsafeFromNumber (LocatedToken (Lexer.Number n) _) = n

natural :: Prod r e LocatedToken Natural
natural = fmap unsafeFromNatural (satisfy isNatural)
  where
    isNatural (LocatedToken (Lexer.NaturalLit _) _) = True
    isNatural  _                                    = False

    unsafeFromNatural (LocatedToken (Lexer.NaturalLit n) _) = n

double :: Prod r e LocatedToken Double
double = fmap unsafeFromDouble (satisfy isDouble)
  where
    isDouble (LocatedToken (Lexer.DoubleLit _) _) = True
    isDouble  _                                   = False

    unsafeFromDouble (LocatedToken (Lexer.DoubleLit n) _) = n

file :: Prod r e LocatedToken FilePath
file = fmap unsafeFromFile (satisfy isFile)
  where
    isFile (LocatedToken (Lexer.File _) _) = True
    isFile  _                              = False

    unsafeFromFile (LocatedToken (Lexer.File n) _) = n

url :: Prod r e LocatedToken Text
url = fmap unsafeFromURL (satisfy isURL)
  where
    isURL (LocatedToken (Lexer.URL _) _) = True
    isURL  _                             = False

    unsafeFromURL (LocatedToken (Lexer.URL n) _) = n

text :: Prod r e LocatedToken Text
text = fmap unsafeFromText (satisfy isText)
  where
    isText (LocatedToken (Lexer.TextLit _) _) = True
    isText  _                                 = False

    unsafeFromText (LocatedToken (Lexer.TextLit n) _) = n

sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepBy :: Alternative f => f a -> f b -> f [a]
sepBy p sep = sepBy1 p sep <|> pure []

expr :: Grammar r (Prod r Token LocatedToken (Expr Path))
expr = mdo
    expr <- rule
        (   bexpr
        <|> (   Lam
            <$> (match Lexer.Lambda *> match Lexer.OpenParen *> label)
            <*> (match Lexer.Colon *> expr)
            <*> (match Lexer.CloseParen *> match Lexer.Arrow *> expr)
            )
        <|> (   Pi
            <$> (match Lexer.Pi *> match Lexer.OpenParen *> label)
            <*> (match Lexer.Colon *> expr)
            <*> (match Lexer.CloseParen *> match Lexer.Arrow *> expr)
            )
        <|> (   Pi "_"
            <$> bexpr
            <*> (match Lexer.Arrow *> expr)
            )
        <|> (    IntegerLit . negate . fromIntegral
            <$> (match Lexer.Dash *> number)
            )
        <|> (   Lets
            <$> some letExpr
            <*> (match Lexer.In *> expr)
            )
        )

    letExpr <- rule
        (   Let
        <$> (match Lexer.Let *> label)
        <*> many argExpr
        <*> (match Lexer.Equals *> expr)
        )

    argExpr <- rule
        (   (,)
        <$> (match Lexer.OpenParen *> label)
        <*> (match Lexer.Colon *> expr <* match Lexer.CloseParen)
        )

    bexpr <- rule
        (   (App <$> bexpr <*> aexpr)
        <|> (   List
            <$> (match Lexer.OpenBracket *> bexpr <* match Lexer.CloseBracket)
            )
        <|> (   BoolAnd
            <$> bexpr
            <*> (match Lexer.And *> bexpr)
            )
        <|> (   BoolOr
            <$> bexpr
            <*> (match Lexer.Or *> bexpr)
            )
        <|> (   NaturalPlus
            <$> bexpr
            <*> (match Lexer.Plus *> bexpr)
            )
        <|> (   NaturalTimes
            <$> bexpr
            <*> (match Lexer.Star *> bexpr)
            )
        <|> (   TextAppend
            <$> bexpr
            <*> (match Lexer.DoublePlus *> bexpr)
            )
        <|> aexpr
        )
    aexpr <- rule
        (   (Var <$> label)
        <|> (match Lexer.Type *> pure (Const Star))
        <|> (match Lexer.Box  *> pure (Const Box))
        <|> (match Lexer.Bool *> pure Bool)
        <|> (match Lexer.Natural *> pure Natural)
        <|> (match Lexer.NaturalFold *> pure NaturalFold)
        <|> (match Lexer.Integer *> pure Integer)
        <|> (match Lexer.Double *> pure Double)
        <|> (match Lexer.Text *> pure Text)
        <|> (match Lexer.ListBuild *> pure ListBuild)
        <|> (match Lexer.ListFold *> pure ListFold)
        <|> (BoolLit <$> bool)
        <|> (IntegerLit . fromIntegral <$> number)
        <|> (NaturalLit <$> natural)
        <|> (DoubleLit <$> double)
        <|> (TextLit <$> text)
        <|> (   flip ListLit
            <$> fmap Data.Vector.fromList
                (match Lexer.OpenBracket *> sepBy expr (match Lexer.Comma))
            <*> (match Lexer.Colon *> expr <* match Lexer.CloseBracket)
            )
        <|> recordLit
        <|> record
        <|> (Embed <$> import_)
        <|> (Field <$> aexpr <*> (match Lexer.Dot *> label))
        <|> (match Lexer.OpenParen *> expr <* match Lexer.CloseParen)
        )

    let toRecordLit xs ys = RecordLit (Data.Map.fromList (xs ++ ys))

    recordLit <- rule
        (   match Lexer.OpenBrace
        *>  (   toRecordLit
            <$> many (fieldValue <* match Lexer.Comma)
            <*> (fmap pure fieldValue <|> pure empty)
            )
        <*  match Lexer.CloseBrace
        )

    fieldValue <- rule
        (   (,)
        <$> label
        <*> (match Lexer.Equals *> expr)
        )

    let toRecord xs ys = Record (Data.Map.fromList (xs ++ ys))

    record <- rule
        (   match Lexer.OpenBrace
        *>  (   toRecord
            <$> many (fieldType <* match Lexer.Comma)
            <*> (fmap pure fieldType <|> pure empty)
            )
        <*  match Lexer.CloseBrace
        )

    fieldType <- rule
        (   (,)
        <$> label
        <*> (match Lexer.Colon *> expr)
        )

    import_ <- rule
        (   (File <$> file)
        <|> (URL <$> url)
        )

    return expr

-- | The specific parsing error
data ParseMessage
    -- | Lexing failed, returning the remainder of the text
    = Lexing Text
    -- | Parsing failed, returning the invalid token and the expected tokens
    | Parsing Token [Token]
    deriving (Show)

-- | Structured type for parsing errors
data ParseError = ParseError
    { position     :: Position
    , parseMessage :: ParseMessage
    } deriving (Typeable)

instance Show ParseError where
    show = Text.unpack . toLazyText . build

instance Exception ParseError

instance Buildable ParseError where
    build (ParseError (Lexer.P l c) e) =
            "\n"
        <>  "Line:   " <> build l <> "\n"
        <>  "Column: " <> build c <> "\n"
        <>  "\n"
        <>  case e of
            Lexing r                                     ->
                    "Lexing: \"" <> build remainder <> dots <> "\"\n"
                <>  "\n"
                <>  "Error: Lexing failed\n"
              where
                remainder = Text.takeWhile (/= '\n') (Text.take 64 r)
                dots      = if Text.length r > 64 then "..." else mempty
            Parsing t ts ->
                    "Parsing : " <> build (show t ) <> "\n"
                <>  "Expected: " <> build (show ts) <> "\n"
                <>  "\n"
                <>  "Error: Parsing failed\n"

-- | Parse an `Expr` from `Text` or return a `ParseError` if parsing fails
exprFromText :: Text -> Either ParseError (Expr Path)
exprFromText text = evalState (runExceptT m) (Lexer.P 1 0)
  where
    m = do
        (locatedTokens, mtxt) <- lift (Pipes.toListM' (Lexer.lexExpr text))
        case mtxt of
            Nothing  -> return ()
            Just txt -> do
                pos <- lift get
                throwE (ParseError pos (Lexing txt))
        let (parses, Report _ needed found) =
                fullParses (parser expr) locatedTokens
        case parses of
            parse:_ -> return parse
            []      -> do
                let LocatedToken t pos = case found of
                        lt:_ -> lt
                        _    -> LocatedToken Lexer.EOF (P 0 0)
                throwE (ParseError pos (Parsing t needed))
