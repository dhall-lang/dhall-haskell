{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | This module contains Dhall's parsing logic

module Dhall.Parser (
    -- * Utilities
      exprFromText

    -- * Parsers
    , expr, exprA

    -- * Types
    , Src(..)
    , ParseError(..)
    , Parser(..)
    ) where

import Control.Applicative (Alternative(..), optional)
import Control.Exception (Exception)
import Control.Monad (MonadPlus)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Sequence (ViewL(..))
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Dhall.Core
import Prelude hiding (const, pi)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Text.Parser.Combinators (choice, try, (<?>))
import Text.Parser.Token (IdentifierStyle(..), TokenParsing(..))
import Text.Parser.Token.Highlight (Highlight(..))
import Text.Trifecta
    (CharParsing, DeltaParsing, MarkParsing, Parsing, Result(..))
import Text.Trifecta.Delta (Delta)

import qualified Data.Char
import qualified Data.HashSet
import qualified Data.Map
import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding
import qualified Data.Vector
import qualified Filesystem.Path.CurrentOS
import qualified Text.Parser.Char
import qualified Text.Parser.Combinators
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Style
import qualified Text.PrettyPrint.ANSI.Leijen
import qualified Text.Trifecta

-- | Source code extract
data Src = Src Delta Delta ByteString deriving (Show)

instance Buildable Src where
    build (Src begin _ bytes) =
            build text <> "\n"
        <>  "\n"
        <>  build (show (Text.PrettyPrint.ANSI.Leijen.pretty begin))
        <>  "\n"
      where
        bytes' = Data.ByteString.Lazy.fromStrict bytes

        text = Data.Text.Lazy.strip (Data.Text.Lazy.Encoding.decodeUtf8 bytes')

{-| A `Parser` that is almost identical to
    @"Text.Trifecta".`Text.Trifecta.Parser`@ except treating Haskell-style
    comments as whitespace
-}
newtype Parser a = Parser { unParser :: Text.Trifecta.Parser a }
    deriving
    (   Functor
    ,   Applicative
    ,   Monad
    ,   Alternative
    ,   MonadPlus
    ,   Parsing
    ,   CharParsing
    ,   DeltaParsing
    ,   MarkParsing Delta
    )

instance TokenParsing Parser where
    someSpace =
        Text.Parser.Token.Style.buildSomeSpaceParser 
            (Parser someSpace)
            Text.Parser.Token.Style.haskellCommentStyle

    nesting (Parser m) = Parser (nesting m)

    semi = Parser semi

    highlight h (Parser m) = Parser (highlight h m)

    token parser = do
        r <- parser
        Text.Parser.Token.whiteSpace
        return r

identifierStyle :: IdentifierStyle Parser
identifierStyle = IdentifierStyle
    { _styleName     = "dhall"
    , _styleStart    =
        Text.Parser.Char.oneOf (['A'..'Z'] ++ ['a'..'z'] ++ "_")
    , _styleLetter   =
        Text.Parser.Char.oneOf (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_/")
    , _styleReserved = Data.HashSet.fromList
        [ "let"
        , "in"
        , "Type"
        , "Kind"
        , "forall"
        , "Bool"
        , "True"
        , "False"
        , "merge"
        , "if"
        , "then"
        , "else"
        , "as"
        , "Natural"
        , "Natural/fold"
        , "Natural/build"
        , "Natural/isZero"
        , "Natural/even"
        , "Natural/odd"
        , "Integer"
        , "Double"
        , "Text"
        , "Text/length"
        , "List"
        , "List/build"
        , "List/fold"
        , "List/length"
        , "List/head"
        , "List/last"
        , "List/indexed"
        , "List/reverse"
        , "Optional"
        , "Optional/fold"
        ]
    , _styleHighlight         = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

noted :: Parser (Expr Src a) -> Parser (Expr Src a)
noted parser = do
    before     <- Text.Trifecta.position
    (e, bytes) <- Text.Trifecta.slicedWith (,) parser
    after      <- Text.Trifecta.position
    return (Note (Src before after bytes) e)

toMap :: [(Text, a)] -> Parser (Map Text a)
toMap kvs = do
    let adapt (k, v) = (k, pure v)
    let m = Data.Map.fromListWith (<|>) (fmap adapt kvs)
    let action k vs = case Data.Sequence.viewl vs of
            EmptyL  -> empty
            v :< vs' ->
                if null vs'
                then pure v
                else
                    Text.Parser.Combinators.unexpected
                        ("duplicate field: " ++ Data.Text.Lazy.unpack k)
    Data.Map.traverseWithKey action m

reserve :: String -> Parser ()
reserve string = do
    _ <- Text.Parser.Token.reserve identifierStyle string
    return ()

symbol :: String -> Parser ()
symbol string = do
    _ <- Text.Parser.Token.symbol string
    return ()

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
    a <- p
    b <- optional sep
    case b of
        Nothing -> return [a]
        Just _  -> do
            as <- sepBy p sep
            return (a:as)

stringLiteral :: Parser Builder
stringLiteral = Text.Parser.Token.stringLiteral <|> doubleSingleQuoteString

doubleSingleQuoteString :: Parser Builder
doubleSingleQuoteString = do
    builder <- Text.Parser.Token.token p0
    return (process builder)
  where
    process =
          Data.Text.Lazy.Builder.fromLazyText
        . Data.Text.Lazy.unlines
        . trim
        . Data.Text.Lazy.lines
        . Data.Text.Lazy.Builder.toLazyText

    trim lines_ = map (Data.Text.Lazy.drop shortestIndent) lines_
      where
        isEmpty = Data.Text.Lazy.all Data.Char.isSpace

        nonEmptyLines = filter (not . isEmpty) lines_

        indentLength line =
            Data.Text.Lazy.length
                (Data.Text.Lazy.takeWhile Data.Char.isSpace line)

        shortestIndent = case nonEmptyLines of
            [] -> 0
            _  -> minimum (map indentLength nonEmptyLines)

    p0 = do
        _ <- Text.Parser.Char.string "''"
        p1

    p1 = p2 <|> p3 <|> p4 <|> p5

    p2 = do
        _  <- Text.Parser.Char.text "'''"
        s1 <- p1
        return ("''" <> s1)

    p3 = do
        _ <- Text.Parser.Char.text "''"
        return ""

    p4 = do
        s0 <- Text.Parser.Char.text "'"
        s1 <- p1
        return (Data.Text.Lazy.Builder.fromText s0 <> s1)

    p5 = do
        s0 <- some (Text.Trifecta.satisfy (/= '\''))
        s1 <- p1
        return (Data.Text.Lazy.Builder.fromString s0 <> s1)

lambda :: Parser ()
lambda = symbol "\\" <|> symbol "λ"

pi :: Parser ()
pi = reserve "forall" <|> reserve "∀"

arrow :: Parser ()
arrow = symbol "->" <|> symbol "→"

combine :: Parser ()
combine = symbol "/\\" <|> symbol "∧"

prefer :: Parser ()
prefer = symbol "//" <|> symbol "⫽"

label :: Parser Text
label = Text.Parser.Token.ident identifierStyle <?> "label"

-- | Parser for a top-level Dhall expression
expr :: Parser (Expr Src Path)
expr = exprA import_

-- | Parser for a top-level Dhall expression. The expression is parameterized
-- over any parseable type, allowing the language to be extended as needed.
exprA :: Show a => Parser a -> Parser (Expr Src a)
exprA embedded = choice
    [   noted      exprA0
    ,   noted      exprA1
    ,   noted      exprA2
    ,   noted      exprA3
    ,   noted (try exprA4)
    ,              exprA5
    ]
  where
    exprA0 = do
        lambda
        symbol "("
        a <- label
        symbol ":"
        b <- exprA embedded
        symbol ")"
        arrow
        c <- exprA embedded
        return (Lam a b c)

    exprA1 = do
        reserve "if"
        a <- exprA embedded
        reserve "then"
        b <- exprA embedded
        reserve "else"
        c <- exprA embedded
        return (BoolIf a b c)

    exprA2 = do
        pi
        symbol "("
        a <- label
        symbol ":"
        b <- exprA embedded
        symbol ")"
        arrow
        c <- exprA embedded
        return (Pi a b c)

    exprA3 = do
        reserve "let"
        a <- label
        b <- optional (do
            symbol ":"
            exprA embedded )
        symbol "="
        c <- exprA embedded
        reserve "in"
        d <- exprA embedded
        return (Let a b c d)

    exprA4 = do
        a <- exprC embedded
        arrow
        b <- exprA embedded
        return (Pi "_" a b)

    exprA5 = exprB embedded

exprB :: Show a => Parser a -> Parser (Expr Src a)
exprB embedded = choice
    [   noted      exprB0
    ,   noted (try exprB1)
    ,   noted      exprB2
    ]
  where
    exprB0 = do
        reserve "merge"
        a <- exprE embedded
        b <- exprE embedded
        symbol ":"
        c <- exprD embedded
        return (Merge a b c)

    exprB1 = do
        symbol "["
        a <- elems embedded
        symbol "]"
        symbol ":"
        b <- listLike
        c <- exprE embedded
        return (b c a)

    exprB2 = do
        a <- exprC embedded

        let exprB2A= do
                symbol ":"
                b <- exprA embedded
                return (Annot a b)

        let exprB2B = pure a

        exprB2A <|> exprB2B

listLike :: Parser (Expr Src a -> Vector (Expr Src a) -> Expr Src a)
listLike =
    (   listLike0
    <|> listLike1
    )
  where
    listLike0 = do
        reserve "List"
        return (\a b -> ListLit (Just a) b)

    listLike1 = do
        reserve "Optional"
        return OptionalLit

exprC :: Show a => Parser a -> Parser (Expr Src a)
exprC embedded = exprC0
  where
    chain pA pOp op pB = noted (do
        a <- pA
        try (do _ <- pOp <?> "operator"; b <- pB; return (op a b)) <|> pure a )

    exprC0 = chain  exprC1          (symbol "||") BoolOr       exprC0
    exprC1 = chain  exprC2          (symbol "+" ) NaturalPlus  exprC1
    exprC2 = chain  exprC3          (symbol "++") TextAppend   exprC2
    exprC3 = chain  exprC4          (symbol "&&") BoolAnd      exprC3
    exprC4 = chain  exprC5           combine      Combine      exprC4
    exprC5 = chain  exprC6           prefer       Prefer       exprC5
    exprC6 = chain  exprC7          (symbol "*" ) NaturalTimes exprC6
    exprC7 = chain  exprC8          (symbol "==") BoolEQ       exprC7
    exprC8 = chain (exprD embedded) (symbol "!=") BoolNE       exprC8

-- We can't use left-recursion to define `exprD` otherwise the parser will
-- loop infinitely. However, I'd still like to use left-recursion in the
-- definition because left recursion greatly simplifies the use of `noted`.  The
-- work-around is to parse in two phases:
--
-- * First, parse to count how many arguments the function is applied to
-- * Second, restart the parse using left recursion bounded by the number of
--   arguments
exprD :: Show a => Parser a -> Parser (Expr Src a)
exprD embedded = do
    e  <- noted (exprE embedded)
    es <- many (noted (try (exprE embedded)))
    let app nL@(Note (Src before _ bytesL) _) nR@(Note (Src _ after bytesR) _) =
            Note (Src before after (bytesL <> bytesR)) (App nL nR)
        app _ _ = Dhall.Core.internalError
            ("Dhall.Parser.exprD: foldl1 app (" <> Data.Text.pack (show es) <> ")")
    return (Data.List.foldl1 app (e:es))

exprE :: Show a => Parser a -> Parser (Expr Src a)
exprE embedded = noted (do
    a <- exprF embedded
    b <- many (try (do
        symbol "."
        label ))
    return (Data.List.foldl Field a b) )

exprF :: Show a => Parser a -> Parser (Expr Src a)
exprF embedded = choice
    [   noted (try exprF26)
    ,   noted (try exprF25)
    ,   noted      exprF24
    ,   noted      exprF27
    ,   noted (try exprF28)
    ,   noted      exprF29
    ,   noted (try exprF30)
    ,   noted      exprF31
    ,   noted      exprF32
    ,   noted      exprF33
    ,   (choice
            [   noted      exprF03
            ,   noted      exprF04
            ,   noted      exprF05
            ,   noted      exprF06
            ,   noted      exprF07
            ,   noted      exprF35
            ,   noted      exprF12
            ,   noted      exprF13
            ,   noted      exprF14
            ,   noted      exprF15
            ,   noted      exprF16
            ,   noted      exprF17
            ,   noted      exprF18
            ,   noted      exprF20
            ,   noted      exprF21
            ,   noted      exprF19
            ,   noted      exprF02
            ,   noted      exprF08
            ,   noted      exprF09
            ,   noted      exprF10
            ,   noted      exprF11
            ,   noted      exprF22
            ,   noted      exprF23
            ,   noted      exprF01
            ]
        ) <?> "built-in value"
    ,   noted      exprF00
    ,              exprF34
    ]
  where
    exprF00 = do
        a <- var
        return (Var a)

    exprF01 = do
        a <- const
        return (Const a)

    exprF02 = do
        reserve "Natural"
        return Natural

    exprF03 = do
        reserve "Natural/fold"
        return NaturalFold

    exprF04 = do
        reserve "Natural/build"
        return NaturalBuild

    exprF05 = do
        reserve "Natural/isZero"
        return NaturalIsZero

    exprF06 = do
        reserve "Natural/even"
        return NaturalEven

    exprF07 = do
        reserve "Natural/odd"
        return NaturalOdd

    exprF08 = do
        reserve "Integer"
        return Integer

    exprF09 = do
        reserve "Double"
        return Double

    exprF10 = do
        reserve "Text"
        return Text

    exprF11 = do
        reserve "List"
        return List

    exprF12 = do
        reserve "List/build"
        return ListBuild

    exprF13 = do
        reserve "List/fold"
        return ListFold

    exprF14 = do
        reserve "List/length"
        return ListLength

    exprF15 = do
        reserve "List/head"
        return ListHead

    exprF16 = do
        reserve "List/last"
        return ListLast

    exprF17 = do
        reserve "List/indexed"
        return ListIndexed

    exprF18 = do
        reserve "List/reverse"
        return ListReverse

    exprF19 = do
        reserve "Optional"
        return Optional

    exprF20 = do
        reserve "Optional/fold"
        return OptionalFold

    exprF21 = do
        reserve "Bool"
        return Bool

    exprF22 = do
        reserve "True"
        return (BoolLit True)

    exprF23 = do
        reserve "False"
        return (BoolLit False)

    exprF24 = do
        a <- Text.Parser.Token.integer
        return (IntegerLit a)

    exprF25 = (do
        _ <- Text.Parser.Char.char '+'
        a <- Text.Parser.Token.natural
        return (NaturalLit (fromIntegral a)) ) <?> "natural"

    exprF26 = do
        sign <-  fmap (\_ -> negate) (Text.Parser.Char.char '-')
             <|> fmap (\_ -> id    ) (Text.Parser.Char.char '+')
             <|> pure id
        a <- Text.Parser.Token.double
        return (DoubleLit (sign a))

    exprF27 = do
        a <- stringLiteral
        return (TextLit a)

    exprF28 = record embedded <?> "record type"

    exprF29 = recordLit embedded <?> "record literal"

    exprF30 = union embedded <?> "union type"

    exprF31 = unionLit embedded <?> "union literal"

    exprF32 = listLit embedded <?> "list literal"

    exprF33 = do
        a <- embedded <?> "import"
        return (Embed a)

    exprF34 = do
        symbol "("
        a <- exprA embedded
        symbol ")"
        return a

    exprF35 = do
        reserve "Text/length"
        return TextLength

const :: Parser Const
const = const0
    <|> const1
  where
    const0 = do
        reserve "Type"
        return Type

    const1 = do
        reserve "Kind"
        return Kind

var :: Parser Var
var = do
    a <- label
    m <- optional (do
        symbol "@"
        Text.Parser.Token.natural )
    let b = case m of
            Just r  -> r
            Nothing -> 0
    return (V a b)

elems :: Show a => Parser a -> Parser (Vector (Expr Src a))
elems embedded = do
    a <- sepBy (exprA embedded) (symbol ",")
    return (Data.Vector.fromList a)

recordLit :: Show a => Parser a -> Parser (Expr Src a)
recordLit embedded =
        recordLit0
    <|> recordLit1
  where
    recordLit0 = do
        symbol "{=}"
        return (RecordLit (Data.Map.fromList []))

    recordLit1 = do
        symbol "{"
        a <- fieldValues embedded
        b <- toMap a
        symbol "}"
        return (RecordLit b)

fieldValues :: Show a => Parser a -> Parser [(Text, Expr Src a)]
fieldValues embedded = sepBy1 (fieldValue embedded) (symbol ",")

fieldValue :: Show a => Parser a -> Parser (Text, Expr Src a)
fieldValue embedded = do
    a <- label
    symbol "="
    b <- exprA embedded
    return (a, b)

record :: Show a => Parser a -> Parser (Expr Src a)
record embedded = do
    symbol "{"
    a <- fieldTypes embedded
    b <- toMap a
    symbol "}"
    return (Record b)

fieldTypes :: Show a => Parser a -> Parser [(Text, Expr Src a)]
fieldTypes embedded = sepBy (fieldType embedded) (symbol ",")

fieldType :: Show a => Parser a -> Parser (Text, Expr Src a)
fieldType embedded = do
    a <- label
    symbol ":"
    b <- exprA embedded
    return (a, b)

union :: Show a => Parser a -> Parser (Expr Src a)
union embedded = do
    symbol "<"
    a <- alternativeTypes embedded
    b <- toMap a
    symbol ">"
    return (Union b)

alternativeTypes :: Show a => Parser a -> Parser [(Text, Expr Src a)]
alternativeTypes embedded = sepBy (alternativeType embedded) (symbol "|")

alternativeType :: Show a => Parser a -> Parser (Text, Expr Src a)
alternativeType embedded = do
    a <- label
    symbol ":"
    b <- exprA embedded
    return (a, b)

unionLit :: Show a => Parser a -> Parser (Expr Src a)
unionLit embedded =
        try unionLit0
    <|>     unionLit1
  where
    unionLit0 = do
        symbol "<"
        a <- label
        symbol "="
        b <- exprA embedded
        symbol ">"
        return (UnionLit a b Data.Map.empty)

    unionLit1 = do
        symbol "<"
        a <- label
        symbol "="
        b <- exprA embedded
        symbol "|"
        c <- alternativeTypes embedded
        d <- toMap c
        symbol ">"
        return (UnionLit a b d)

listLit :: Show a => Parser a -> Parser (Expr Src a)
listLit embedded = do
    symbol "["
    a <- elems embedded
    symbol "]"
    return (ListLit Nothing a)

import_ :: Parser Path
import_ = do
    pathType <- file <|> url <|> env
    Text.Parser.Token.whiteSpace
    let rawText = do
            _ <- reserve "as"
            _ <- reserve "Text"
            return RawText
    pathMode <- rawText <|> pure Code
    return (Path {..})

pathChar :: Char -> Bool
pathChar c = not (Data.Char.isSpace c || c == '(' || c == ')')

file :: Parser PathType
file =  try (token file0)
    <|>      token file1
    <|>      token file2
    <|>      token file3
  where
    file0 = do
        a <- Text.Parser.Char.string "/"
        b <- many (Text.Parser.Char.satisfy pathChar)
        case b of
            '\\':_ -> empty -- So that "/\" parses as the operator and not a path
            '/' :_ -> empty -- So that "//" parses as the operator and not a path
            _      -> return ()
        return (File Homeless (Filesystem.Path.CurrentOS.decodeString (a <> b)))

    file1 = do
        a <- Text.Parser.Char.string "./"
        b <- many (Text.Parser.Char.satisfy pathChar)
        return (File Homeless (Filesystem.Path.CurrentOS.decodeString (a <> b)))

    file2 = do
        a <- Text.Parser.Char.string "../"
        b <- many (Text.Parser.Char.satisfy pathChar)
        return (File Homeless (Filesystem.Path.CurrentOS.decodeString (a <> b)))

    file3 = do
        _ <- Text.Parser.Char.string "~"
        _ <- some (Text.Parser.Char.string "/")
        b <- many (Text.Parser.Char.satisfy pathChar)
        return (File Home (Filesystem.Path.CurrentOS.decodeString b))

url :: Parser PathType
url =   try url0
    <|> url1
  where
    url0 = do
        a <- Text.Parser.Char.string "https://"
        b <- many (Text.Parser.Char.satisfy pathChar)
        return (URL (Data.Text.Lazy.pack (a <> b)))

    url1 = do
        a <- Text.Parser.Char.string "http://"
        b <- many (Text.Parser.Char.satisfy pathChar)
        return (URL (Data.Text.Lazy.pack (a <> b)))

env :: Parser PathType
env = do
    _ <- Text.Parser.Char.string "env:"
    a <- many (Text.Parser.Char.satisfy pathChar)
    return (Env (Data.Text.Lazy.pack a))

-- | A parsing error
newtype ParseError = ParseError Doc deriving (Typeable)

instance Show ParseError where
    show (ParseError doc) =
      "\n\ESC[1;31mError\ESC[0m: Invalid input\n\n" <> show doc

instance Exception ParseError

-- | Parse an expression from `Text` containing a Dhall program
exprFromText :: Delta -> Text -> Either ParseError (Expr Src Path)
exprFromText delta text = case result of
    Success r       -> Right r
    Failure errInfo -> Left (ParseError (Text.Trifecta._errDoc errInfo))
  where
    string = Data.Text.Lazy.unpack text

    parser = unParser (do
        Text.Parser.Token.whiteSpace
        r <- expr
        Text.Parser.Combinators.eof
        return r )

    result = Text.Trifecta.parseString parser delta string
