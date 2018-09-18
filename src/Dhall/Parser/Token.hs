{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Parse Dhall tokens. Even though we don't have a tokenizer per-se this
---  module is useful for keeping some small parsing utilities.
module Dhall.Parser.Token (
    whitespace,
    bashEnvironmentVariable,
    posixEnvironmentVariable,
    file_,
    label,
    labels,
    httpRaw,
    hexdig,
    identifier,
    hexNumber,
    doubleLiteral,
    naturalLiteral,
    integerLiteral,
    _Optional,
    _if,
    _then,
    _else,
    _let,
    _in,
    _as,
    _using,
    _merge,
    _constructors,
    _Some,
    _None,
    _NaturalFold,
    _NaturalBuild,
    _NaturalIsZero,
    _NaturalEven,
    _NaturalOdd,
    _NaturalToInteger,
    _NaturalShow,
    _IntegerShow,
    _IntegerToDouble,
    _DoubleShow,
    _ListBuild,
    _ListFold,
    _ListLength,
    _ListHead,
    _ListLast,
    _ListIndexed,
    _ListReverse,
    _OptionalFold,
    _OptionalBuild,
    _Bool,
    _Natural,
    _Integer,
    _Double,
    _Text,
    _List,
    _True,
    _False,
    _Type,
    _Kind,
    _equal,
    _or,
    _plus,
    _textAppend,
    _listAppend,
    _and,
    _times,
    _doubleEqual,
    _notEqual,
    _dot,
    _openBrace,
    _closeBrace,
    _openBracket,
    _closeBracket,
    _openAngle,
    _closeAngle,
    _bar,
    _comma,
    _openParens,
    _closeParens,
    _colon,
    _at,
    _missing,
    _importAlt,
    _combine,
    _combineTypes,
    _prefer,
    _lambda,
    _forall,
    _arrow,
    ) where

import           Dhall.Parser.Combinators

import Control.Applicative (Alternative(..), optional)
import Data.Functor (void)
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import Data.Text (Text)
import Dhall.Core
import Prelude hiding (const, pi)
import Text.Parser.Combinators (choice, try, (<?>))

import qualified Control.Monad
import qualified Data.Char
import qualified Data.HashSet
import qualified Data.List.NonEmpty
import qualified Data.Set
import qualified Data.Text
import qualified Text.Parser.Char
import qualified Text.Parser.Combinators

import Data.Scientific (Scientific)
import Numeric.Natural (Natural)
import Prelude hiding (const, pi)

import qualified Text.Parser.Token


whitespace :: Parser ()
whitespace = Text.Parser.Combinators.skipMany whitespaceChunk

nonemptyWhitespace :: Parser ()
nonemptyWhitespace = Text.Parser.Combinators.skipSome whitespaceChunk

alpha :: Char -> Bool
alpha c = ('\x41' <= c && c <= '\x5A') || ('\x61' <= c && c <= '\x7A')

digit :: Char -> Bool
digit c = '\x30' <= c && c <= '\x39'

hexdig :: Char -> Bool
hexdig c =
        ('0' <= c && c <= '9')
    ||  ('A' <= c && c <= 'F')
    ||  ('a' <= c && c <= 'f')

doubleLiteral :: Parser Scientific
doubleLiteral = (do
    sign <-  fmap (\_ -> negate) (Text.Parser.Char.char '-')
         <|> pure id
    a    <-  Text.Parser.Token.scientific
    return (sign a) ) <?> "double literal"

integerLiteral :: Parser Integer
integerLiteral = (do
    let positive = fmap (\_ -> id    ) (Text.Parser.Char.char '+')
    let negative = fmap (\_ -> negate) (Text.Parser.Char.char '-')
    sign <- positive <|> negative
    a <- Text.Parser.Token.natural
    return (sign a) ) <?> "integer literal"

naturalLiteral :: Parser Natural
naturalLiteral = (do
    a <- Text.Parser.Token.natural
    return (fromIntegral a) ) <?> "natural literal"

identifier :: Parser Var
identifier = do
    x <- label

    let indexed = do
            _ <- Text.Parser.Char.char '@'
            Text.Parser.Token.natural

    n <- indexed <|> pure 0
    return (V x n)

whitespaceChunk :: Parser ()
whitespaceChunk =
    choice
        [ void (Text.Parser.Char.satisfy predicate)
        , void (Text.Parser.Char.text "\r\n")
        , lineComment
        , blockComment
        ] <?> "whitespace"
  where
    predicate c = c == ' ' || c == '\t' || c == '\n'

hexNumber :: Parser Int
hexNumber = choice [ hexDigit, hexUpper, hexLower ]
  where
    hexDigit = do
        c <- Text.Parser.Char.satisfy predicate
        return (Data.Char.ord c - Data.Char.ord '0')
      where
        predicate c = '0' <= c && c <= '9'

    hexUpper = do
        c <- Text.Parser.Char.satisfy predicate
        return (10 + Data.Char.ord c - Data.Char.ord 'A')
      where
        predicate c = 'A' <= c && c <= 'F'

    hexLower = do
        c <- Text.Parser.Char.satisfy predicate
        return (10 + Data.Char.ord c - Data.Char.ord 'a')
      where
        predicate c = 'a' <= c && c <= 'f'

lineComment :: Parser ()
lineComment = do
    _ <- Text.Parser.Char.text "--"
    Text.Parser.Combinators.skipMany notEndOfLine
    endOfLine
    return ()
  where
    endOfLine =
            void (Text.Parser.Char.char '\n'  )
        <|> void (Text.Parser.Char.text "\r\n")

    notEndOfLine = void (Text.Parser.Char.satisfy predicate)
      where
        predicate c = ('\x20' <= c && c <= '\x10FFFF') || c == '\t'

blockComment :: Parser ()
blockComment = do
    _ <- Text.Parser.Char.text "{-"
    blockCommentContinue

blockCommentChunk :: Parser ()
blockCommentChunk =
    choice
        [ blockComment  -- Nested block comment
        , character
        , endOfLine
        ]
  where
    character = void (Text.Parser.Char.satisfy predicate)
      where
        predicate c = '\x20' <= c && c <= '\x10FFFF' || c == '\n' || c == '\t'

    endOfLine = void (Text.Parser.Char.text "\r\n")

blockCommentContinue :: Parser ()
blockCommentContinue = endOfComment <|> continue
  where
    endOfComment = void (Text.Parser.Char.text "-}")

    continue = do
        blockCommentChunk
        blockCommentContinue


simpleLabel :: Parser Text
simpleLabel = try (do
    c    <- Text.Parser.Char.satisfy headCharacter
    rest <- Dhall.Parser.Combinators.takeWhile tailCharacter
    let text = Data.Text.cons c rest
    Control.Monad.guard (not (Data.HashSet.member text reservedIdentifiers))
    return text )
  where
    headCharacter c = alpha c || c == '_'

    tailCharacter c = alpha c || digit c || c == '_' || c == '-' || c == '/'

backtickLabel :: Parser Text
backtickLabel = do
    _ <- Text.Parser.Char.char '`'
    t <- takeWhile1 predicate
    _ <- Text.Parser.Char.char '`'
    return t
  where
    predicate c = alpha c || digit c || elem c ("$-/_:." :: String)

labels :: Parser (Set Text)
labels = do
    _openBrace
    xs <- nonEmptyLabels <|> emptyLabels
    _closeBrace
    return xs
  where
    emptyLabels = pure Data.Set.empty

    nonEmptyLabels = do
        x  <- label
        xs <- many (do _ <- _comma; label)
        noDuplicates (x : xs)


label :: Parser Text
label = (do
    t <- backtickLabel <|> simpleLabel
    whitespace
    return t ) <?> "label"

bashEnvironmentVariable :: Parser Text
bashEnvironmentVariable = satisfy predicate0 <> star (satisfy predicate1)
  where
    predicate0 c = alpha c || c == '_'

    predicate1 c = alpha c || digit c || c == '_'

posixEnvironmentVariable :: Parser Text
posixEnvironmentVariable = plus posixEnvironmentVariableCharacter

posixEnvironmentVariableCharacter :: Parser Text
posixEnvironmentVariableCharacter =
    ("\\" <> satisfy predicate0) <|> satisfy predicate1
  where
    predicate0 c = c `elem` ("\"\\abfnrtv" :: String)

    predicate1 c =
            ('\x20' <= c && c <= '\x21')
        ||  ('\x23' <= c && c <= '\x3C')
        ||  ('\x3E' <= c && c <= '\x5B')
        ||  ('\x5D' <= c && c <= '\x7E')

pathCharacter :: Char -> Bool
pathCharacter c =
        ('\x21' <= c && c <= '\x22')
    ||  ('\x24' <= c && c <= '\x27')
    ||  ('\x2A' <= c && c <= '\x2B')
    ||  ('\x2D' <= c && c <= '\x2E')
    ||  ('\x30' <= c && c <= '\x3B')
    ||  c == '\x3D'
    ||  ('\x40' <= c && c <= '\x5A')
    ||  ('\x5E' <= c && c <= '\x7A')
    ||  c == '\x7C'
    ||  c == '\x7E'

pathComponent :: Parser Text
pathComponent = do
    _      <- "/" :: Parser Text
    string <- some (Text.Parser.Char.satisfy pathCharacter)

    return (Data.Text.pack string)

file_ :: Parser File
file_ = do
    path <- Data.List.NonEmpty.some1 pathComponent

    let directory = Directory (reverse (Data.List.NonEmpty.init path))
    let file      = Data.List.NonEmpty.last path

    return (File {..})

scheme_ :: Parser Scheme
scheme_ =
        ("http" :: Parser Text)
    *>  ((("s" :: Parser Text) *> pure HTTPS) <|> pure HTTP)
    <*  ("://" :: Parser Text)

httpRaw :: Parser URL
httpRaw = do
    scheme    <- scheme_
    authority <- authority_
    path      <- file_
    query     <- optional (("?" :: Parser Text) *> query_)
    fragment  <- optional (("#" :: Parser Text) *> fragment_)

    let headers = Nothing

    return (URL {..})

authority_ :: Parser Text
authority_ = option (try (userinfo <> "@")) <> host <> option (":" <> port)

userinfo :: Parser Text
userinfo = star (satisfy predicate <|> pctEncoded)
  where
    predicate c = unreserved c || subDelims c || c == ':'

host :: Parser Text
host = choice [ ipLiteral, ipV4Address, regName ]

port :: Parser Text
port = star (satisfy digit)

ipLiteral :: Parser Text
ipLiteral = "[" <> (ipV6Address <|> ipVFuture) <> "]"

ipVFuture :: Parser Text
ipVFuture = "v" <> plus (satisfy hexdig) <> "." <> plus (satisfy predicate)
  where
    predicate c = unreserved c || subDelims c || c == ':'

ipV6Address :: Parser Text
ipV6Address =
    choice
        [ try alternative0
        , try alternative1
        , try alternative2
        , try alternative3
        , try alternative4
        , try alternative5
        , try alternative6
        , try alternative7
        ,     alternative8
        ]
  where
    alternative0 = count 6 (h16 <> ":") <> ls32

    alternative1 = "::" <> count 5 (h16 <> ":") <> ls32

    alternative2 = option h16 <> "::" <> count 4 (h16 <> ":") <> ls32

    alternative3 =
            option (range 0 1 (h16 <> ":") <> h16)
        <>  "::"
        <>  count 3 (h16 <> ":")
        <>  ls32

    alternative4 =
            option (range 0 2 (h16 <> ":") <> h16)
        <>  "::"
        <>  count 2 (h16 <> ":")
        <>  ls32

    alternative5 =
        option (range 0 3 (h16 <> ":") <> h16) <> "::" <> h16 <> ":" <> ls32

    alternative6 =
        option (range 0 4 (h16 <> ":") <> h16) <> "::" <> ls32

    alternative7 =
        option (range 0 5 (h16 <> ":") <> h16) <> "::" <> h16

    alternative8 =
        option (range 0 6 (h16 <> ":") <> h16) <> "::"

h16 :: Parser Text
h16 = range 1 3 (satisfy hexdig)

ls32 :: Parser Text
ls32 = (h16 <> ":" <> h16) <|> ipV4Address

ipV4Address :: Parser Text
ipV4Address = decOctet <> "." <> decOctet <> "." <> decOctet <> "." <> decOctet

decOctet :: Parser Text
decOctet =
    choice
        [ try alternative4
        , try alternative3
        , try alternative2
        , try alternative1
        ,     alternative0
        ]
  where
    alternative0 = satisfy digit

    alternative1 = satisfy predicate <> satisfy digit
      where
        predicate c = '\x31' <= c && c <= '\x39'

    alternative2 = "1" <> count 2 (satisfy digit)

    alternative3 = "2" <> satisfy predicate <> satisfy digit
      where
        predicate c = '\x30' <= c && c <= '\x34'

    alternative4 = "25" <> satisfy predicate
      where
        predicate c = '\x30' <= c && c <= '\x35'

regName :: Parser Text
regName = star (satisfy predicate <|> pctEncoded)
  where
    predicate c = unreserved c || subDelims c

pchar :: Parser Text
pchar = satisfy predicate <|> pctEncoded
  where
    predicate c = unreserved c || subDelims c || c == ':' || c == '@'

query_ :: Parser Text
query_ = star (pchar <|> satisfy predicate)
  where
    predicate c = c == '/' || c == '?'

fragment_ :: Parser Text
fragment_ = star (pchar <|> satisfy predicate)
  where
    predicate c = c == '/' || c == '?'

pctEncoded :: Parser Text
pctEncoded = "%" <> count 2 (satisfy hexdig)

subDelims :: Char -> Bool
subDelims c = c `elem` ("!$&'()*+,;=" :: String)

unreserved :: Char -> Bool
unreserved c =
    alpha c || digit c || c == '-' || c == '.' || c == '_' || c == '~'

reserved :: Data.Text.Text -> Parser ()
reserved x = do _ <- Text.Parser.Char.text x; whitespace

reservedChar :: Char -> Parser ()
reservedChar c = do _ <- Text.Parser.Char.char c; whitespace

keyword :: Data.Text.Text -> Parser ()
keyword x = try (do _ <- Text.Parser.Char.text x; nonemptyWhitespace)

_if :: Parser ()
_if = keyword "if"

_then :: Parser ()
_then = keyword "then"

_else :: Parser ()
_else = keyword "else"

_let :: Parser ()
_let = keyword "let"

_in :: Parser ()
_in = keyword "in"

_as :: Parser ()
_as = keyword "as"

_using :: Parser ()
_using = keyword "using"

_merge :: Parser ()
_merge = keyword "merge"

_constructors :: Parser ()
_constructors = keyword "constructors"

_Some :: Parser ()
_Some = reserved "Some"

_None :: Parser ()
_None = reserved "None"

_NaturalFold :: Parser ()
_NaturalFold = reserved "Natural/fold"

_NaturalBuild :: Parser ()
_NaturalBuild = reserved "Natural/build"

_NaturalIsZero :: Parser ()
_NaturalIsZero = reserved "Natural/isZero"

_NaturalEven :: Parser ()
_NaturalEven = reserved "Natural/even"

_NaturalOdd :: Parser ()
_NaturalOdd = reserved "Natural/odd"

_NaturalToInteger :: Parser ()
_NaturalToInteger = reserved "Natural/toInteger"

_NaturalShow :: Parser ()
_NaturalShow = reserved "Natural/show"

_IntegerShow :: Parser ()
_IntegerShow = reserved "Integer/show"

_IntegerToDouble :: Parser ()
_IntegerToDouble = reserved "Integer/toDouble"

_DoubleShow :: Parser ()
_DoubleShow = reserved "Double/show"

_ListBuild :: Parser ()
_ListBuild = reserved "List/build"

_ListFold :: Parser ()
_ListFold = reserved "List/fold"

_ListLength :: Parser ()
_ListLength = reserved "List/length"

_ListHead :: Parser ()
_ListHead = reserved "List/head"

_ListLast :: Parser ()
_ListLast = reserved "List/last"

_ListIndexed :: Parser ()
_ListIndexed = reserved "List/indexed"

_ListReverse :: Parser ()
_ListReverse = reserved "List/reverse"

_OptionalFold :: Parser ()
_OptionalFold = reserved "Optional/fold"

_OptionalBuild :: Parser ()
_OptionalBuild = reserved "Optional/build"

_Bool :: Parser ()
_Bool = reserved "Bool"

_Optional :: Parser ()
_Optional = reserved "Optional"

_Natural :: Parser ()
_Natural = reserved "Natural"

_Integer :: Parser ()
_Integer = reserved "Integer"

_Double :: Parser ()
_Double = reserved "Double"

_Text :: Parser ()
_Text = reserved "Text"

_List :: Parser ()
_List = reserved "List"

_True :: Parser ()
_True = reserved "True"

_False :: Parser ()
_False = reserved "False"

_Type :: Parser ()
_Type = reserved "Type"

_Kind :: Parser ()
_Kind = reserved "Kind"

_equal :: Parser ()
_equal = reservedChar '='

_or :: Parser ()
_or = reserved "||"

_plus :: Parser ()
_plus = reservedChar '+'

_textAppend :: Parser ()
_textAppend = reserved "++"

_listAppend :: Parser ()
_listAppend = reservedChar '#'

_and :: Parser ()
_and = reserved "&&"

_times :: Parser ()
_times = reservedChar '*'

_doubleEqual :: Parser ()
_doubleEqual = reserved "=="

_notEqual :: Parser ()
_notEqual = reserved "!="

_dot :: Parser ()
_dot = reservedChar '.'

_openBrace :: Parser ()
_openBrace = reservedChar '{'

_closeBrace :: Parser ()
_closeBrace = reservedChar '}'

_openBracket :: Parser ()
_openBracket = reservedChar '['

_closeBracket :: Parser ()
_closeBracket = reservedChar ']'

_openAngle :: Parser ()
_openAngle = reservedChar '<'

_closeAngle :: Parser ()
_closeAngle = reservedChar '>'

_bar :: Parser ()
_bar = reservedChar '|'

_comma :: Parser ()
_comma = reservedChar ','

_openParens :: Parser ()
_openParens = reservedChar '('

_closeParens :: Parser ()
_closeParens = reservedChar ')'

_colon :: Parser ()
_colon = reservedChar ':'

_at :: Parser ()
_at = reservedChar '@'

_missing :: Parser ()
_missing = reserved "missing"

_importAlt :: Parser ()
_importAlt = reservedChar '?'

_combine :: Parser ()
_combine = do
    void (Text.Parser.Char.char '∧' <?> "\"∧\"") <|> void (Text.Parser.Char.text "/\\")
    whitespace

_combineTypes :: Parser ()
_combineTypes = do
    void (Text.Parser.Char.char '⩓' <?> "\"⩓\"") <|> void (Text.Parser.Char.text "//\\\\")
    whitespace

_prefer :: Parser ()
_prefer = do
    void (Text.Parser.Char.char '⫽' <?> "\"⫽\"") <|> void (Text.Parser.Char.text "//")
    whitespace

_lambda :: Parser ()
_lambda = do
    _ <- Text.Parser.Char.satisfy predicate
    whitespace
  where
    predicate 'λ'  = True
    predicate '\\' = True
    predicate _    = False

_forall :: Parser ()
_forall = do
    void (Text.Parser.Char.char '∀' <?> "\"∀\"") <|> void (Text.Parser.Char.text "forall")
    whitespace

_arrow :: Parser ()
_arrow = do
    void (Text.Parser.Char.char '→' <?> "\"→\"") <|> void (Text.Parser.Char.text "->")
    whitespace
