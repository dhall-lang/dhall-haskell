{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | This module contains Dhall's parsing logic

module Dhall.Parser (
    -- * Utilities
      exprFromText
    , exprAndHeaderFromText

    -- * Parsers
    , expr, exprA

    -- * Types
    , Src(..)
    , ParseError(..)
    , Parser(..)
    ) where

import Control.Applicative (Alternative(..), liftA2, optional)
import Control.Exception (Exception)
import Control.Monad (MonadPlus)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Sequence (ViewL(..))
import Data.String (IsString(..))
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Typeable (Typeable)
import Dhall.Core
import Numeric.Natural (Natural)
import Prelude hiding (const, pi)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Text.Parser.Combinators (choice, try, (<?>))
import Text.Parser.Token (IdentifierStyle(..), TokenParsing(..))
import Text.Parser.Token.Highlight (Highlight(..))
import Text.Trifecta
    (CharParsing, DeltaParsing, MarkParsing, Parsing, Result(..))
import Text.Trifecta.Delta (Delta)

import qualified Control.Monad
import qualified Data.Char
import qualified Data.HashSet
import qualified Data.Map
import qualified Data.ByteString.Lazy
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Encoding
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

-- TODO: Deal with variables not being confused with reserved identifiers
-- TODO: Fix grammar for block comments

-- | Source code extract
data Src = Src Delta Delta ByteString deriving (Eq, Show)

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

instance Monoid a => Monoid (Parser a) where
    mempty = pure mempty

    mappend = liftA2 mappend

instance IsString a => IsString (Parser a) where
    fromString x = do
        a <- Text.Parser.Char.string x
        return (fromString a)

instance TokenParsing Parser where
    someSpace =
        Text.Parser.Token.Style.buildSomeSpaceParser
            (Parser someSpace)
            Text.Parser.Token.Style.haskellCommentStyle

    nesting (Parser m) = Parser (nesting m)

    semi = Parser semi

    highlight h (Parser m) = Parser (highlight h m)

identifierStyle :: IdentifierStyle Parser
identifierStyle = IdentifierStyle
    { _styleName     = "dhall"
    , _styleStart    =
        Text.Parser.Char.oneOf (['A'..'Z'] ++ ['a'..'z'] ++ "_")
    , _styleLetter   =
        Text.Parser.Char.oneOf (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_-/")
    , _styleReserved = reservedIdentifiers
    , _styleHighlight         = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

noted :: Parser (Expr Src a) -> Parser (Expr Src a)
noted parser = do
    before     <- Text.Trifecta.position
    (e, bytes) <- Text.Trifecta.slicedWith (,) parser
    after      <- Text.Trifecta.position
    return (Note (Src before after bytes) e)

--------

count :: Monoid a => Int -> Parser a -> Parser a
count n parser = fmap mconcat (Control.Monad.replicateM n parser)

range :: Monoid a => Int -> Int -> Parser a -> Parser a
range minBound maxMatches parser = do
    xs <- count minBound parser
    ys <- loop maxMatches
    return (xs <> ys)
  where
    loop 0 = return mempty
    loop n =
            (do x <- parser; xs <- loop (n - 1); return (x <> xs))
        <|> return mempty

option :: (Alternative f, Monoid a) => f a -> f a
option p = p <|> pure mempty

star :: (Alternative f, Monoid a) => f a -> f a
star p = fmap mconcat (many p)

plus :: (Alternative f, Monoid a) => f a -> f a
plus p = fmap mconcat (some p)

satisfy :: (Char -> Bool) -> Parser Builder
satisfy predicate =
    fmap Data.Text.Lazy.Builder.singleton (Text.Parser.Char.satisfy predicate)

notEndOfLine :: Parser ()
notEndOfLine = void (Text.Parser.Char.satisfy predicate)
  where
    predicate c = ('\x20' <= c && c <= '\x10FFFF') || c == '\t'

notBrace :: Parser ()
notBrace =
        void (Text.Parser.Char.satisfy predicate)
    <|> void (Text.Parser.Char.text "\r\n")
  where
    predicate c =
            ('\x20' <= c && c <= '\x2C')
        ||  ('\x2E' <= c && c <= '\x7A')
        ||  ('\x7C' <= c && c <= '\x10FFFF')
        ||  c == '\n'
        ||  c == '\t'

blockComment :: Parser ()
blockComment = do
    _ <- Text.Parser.Char.text "{-"
    Text.Parser.Combinators.skipMany notBrace
    Text.Parser.Combinators.skipMany $ do
        blockComment
        Text.Parser.Combinators.skipMany notBrace
    _ <- Text.Parser.Char.text "-}"
    return ()

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

whitespaceChunk :: Parser ()
whitespaceChunk =
        void (Text.Parser.Char.satisfy predicate)
    <|> void (Text.Parser.Char.text "\r\n")
    <|> lineComment
    <|> blockComment
  where
    predicate c = c == ' ' || c == '\t' || c == '\n'

whitespace :: Parser ()
whitespace = Text.Parser.Combinators.skipMany whitespaceChunk

alpha :: Char -> Bool
alpha c = ('\x41' <= c && c <= '\x5A') || ('\x61' <= c && c <= '\x7A')

digit :: Char -> Bool
digit c = '\x30' <= c && c <= '\x39'

hexdig :: Char -> Bool
hexdig c =
        ('0' <= c && c <= '9')
    ||  ('A' <= c && c <= 'F')
    ||  ('a' <= c && c <= 'f')

hexNumber :: Parser Int
hexNumber = hexNumber <|> hexUpper <|> hexLower
  where
    hexNumber = do
        c <- Text.Parser.Char.satisfy predicate
        return (Data.Char.ord c - Data.Char.ord '0')
      where
        predicate c = '0' <= c && c <= '9'

    hexUpper = do
        c <- Text.Parser.Char.satisfy predicate
        return (Data.Char.ord c - Data.Char.ord 'A')
      where
        predicate c = 'A' <= c && c <= 'F'

    hexLower = do
        c <- Text.Parser.Char.satisfy predicate
        return (Data.Char.ord c - Data.Char.ord 'a')
      where
        predicate c = 'a' <= c && c <= 'f'

simpleLabel :: Parser Text
simpleLabel = try (do
    c  <- Text.Parser.Char.satisfy headCharacter
    cs <- many (Text.Parser.Char.satisfy tailCharacter)
    let string = c:cs
    Control.Monad.guard (not (Data.HashSet.member string reservedIdentifiers))
    return (Data.Text.Lazy.pack string) )
  where
    headCharacter c = alpha c || c == '_'

    tailCharacter c = alpha c || digit c || c == '_' || c == '-' || c == '/'

complexLabel :: Parser Text
complexLabel = do
    _ <- Text.Parser.Char.char '`'
    t <- simpleLabel
    _ <- Text.Parser.Char.char '`'
    return t

label :: Parser Text
label = do
    t <- complexLabel <|> simpleLabel
    whitespace
    return t

-- | Combine consecutive chunks to eliminate gratuitous appends
textAppend :: Expr Src a -> Expr Src a -> Expr Src a
textAppend (TextLit a) (TextLit b) =
    TextLit (a <> b)
textAppend (TextLit a) (TextAppend (TextLit b) c) =
    TextAppend (TextLit (a <> b)) c
textAppend a b =
    TextAppend a b

doubleQuotedChunk :: Parser a -> Parser (Expr Src a)
doubleQuotedChunk embedded =
        interpolation
    <|> escapeInterpolation
    <|> unescapedCharacter
    <|> escapedCharacter
  where
    interpolation = do
        _ <- Text.Parser.Char.text "${"
        e <- expression embedded
        _ <- Text.Parser.Char.char '}'
        return e

    escapeInterpolation = do
        _ <- Text.Parser.Char.text "''${"
        return (TextLit "''${")

    unescapedCharacter = do
        c <- Text.Parser.Char.satisfy predicate
        return (TextLit (Data.Text.Lazy.Builder.singleton c))
      where
        predicate c =
                ('\x20' <= c && c <= '\x21'    )
            ||  ('\x23' <= c && c <= '\x5B'    )
            ||  ('\x5D' <= c && c <= '\x10FFFF')

    escapedCharacter = do
        _ <- Text.Parser.Char.char '\\'
        c <- (   quotationMark
             <|> dollarSign
             <|> backSlash
             <|> forwardSlash
             <|> backSpace
             <|> formFeed
             <|> lineFeed
             <|> carriageReturn
             <|> tab
             <|> unicode
             )
        return (TextLit (Data.Text.Lazy.Builder.singleton c))
      where
        quotationMark = Text.Parser.Char.char '"'

        dollarSign = Text.Parser.Char.char '$'

        backSlash = Text.Parser.Char.char '\\'

        forwardSlash = Text.Parser.Char.char '/'

        backSpace = do _ <- Text.Parser.Char.char 'b'; return '\b'

        formFeed = do _ <- Text.Parser.Char.char 'f'; return '\f'

        lineFeed = do _ <- Text.Parser.Char.char 'n'; return '\n'

        carriageReturn = do _ <- Text.Parser.Char.char 'r'; return '\r'

        tab = do _ <- Text.Parser.Char.char 't'; return '\t'

        unicode = do
            _  <- Text.Parser.Char.char 'u';
            n0 <- hexNumber
            n1 <- hexNumber
            n2 <- hexNumber
            n3 <- hexNumber
            let n = ((n0 * 16 + n1) * 16 + n2) * 16 + n3
            return (Data.Char.chr n)

doubleQuotedLiteral :: Parser a -> Parser (Expr Src a)
doubleQuotedLiteral embedded = do
    _      <- Text.Parser.Char.char '"'
    chunks <- many (doubleQuotedChunk embedded)
    _      <- Text.Parser.Char.char '"'
    return (foldr textAppend (TextLit "") chunks)

singleQuotedChunk :: Parser a -> Parser (Expr Src a)
singleQuotedChunk embedded =
        escapeSingleQuotes
    <|> interpolation
    <|> escapeInterpolation
    <|> unescapedCharacter
    <|> tab
    <|> endOfLine
  where
        escapeSingleQuotes = fmap TextLit "'''"

        interpolation = do
            _ <- Text.Parser.Char.text "${"
            e <- expression embedded
            _ <- Text.Parser.Char.char '}'
            return e

        escapeInterpolation = fmap TextLit "''${"

        unescapedCharacter = fmap TextLit (satisfy predicate)
          where
            predicate c =
                    ('\x20' <= c && c <= '\x26'    )
                ||  ('\x28' <= c && c <= '\x10FFFF')

        endOfLine = fmap TextLit "\n" <|> fmap TextLit "\r\n"

        tab = do
            _ <- Text.Parser.Char.char '\t'
            return (TextLit "\t")

singleQuotedLiteral :: Parser a -> Parser (Expr Src a)
singleQuotedLiteral embedded = do
    _  <- Text.Parser.Char.text "''"
    a  <- many (singleQuotedChunk embedded)
    b  <- many (try (do
        b <- fmap TextLit "'"
        c <- some (singleQuotedChunk embedded)
        return (foldr textAppend (TextLit "") (b:c)) ))
    _  <- Text.Parser.Char.text "''"
    return (foldr textAppend (TextLit "") (a <> b))

textLiteral :: Parser a -> Parser (Expr Src a)
textLiteral embedded = do
    literal <- doubleQuotedLiteral embedded <|> singleQuotedLiteral embedded
    whitespace
    return literal

reserved :: Data.Text.Text -> Parser ()
reserved x = do _ <- Text.Parser.Char.text x; whitespace

_if :: Parser ()
_if = reserved "if"

_then :: Parser ()
_then = reserved "then"

_else :: Parser ()
_else = reserved "else"

_let :: Parser ()
_let = reserved "let"

_in :: Parser ()
_in = reserved "in"

_as :: Parser ()
_as = reserved "as"

_using :: Parser ()
_using = reserved "using"

_merge :: Parser ()
_merge = reserved "merge"

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
_equal = reserved "="

_or :: Parser ()
_or = reserved "||"

_plus :: Parser ()
_plus = reserved "+"

_textAppend :: Parser ()
_textAppend = reserved "++"

_listAppend :: Parser ()
_listAppend = reserved "#"

_and :: Parser ()
_and = reserved "&&"

_times :: Parser ()
_times = reserved "*"

_doubleEqual :: Parser ()
_doubleEqual = reserved "=="

_notEqual :: Parser ()
_notEqual = reserved "!="

_dot :: Parser ()
_dot = reserved "."

_openBrace :: Parser ()
_openBrace = reserved "{"

_closeBrace :: Parser ()
_closeBrace = reserved "}"

_openBracket :: Parser ()
_openBracket = reserved "["

_closeBracket :: Parser ()
_closeBracket = reserved "]"

_openAngle :: Parser ()
_openAngle = reserved "<"

_closeAngle :: Parser ()
_closeAngle = reserved ">"

_bar :: Parser ()
_bar = reserved "|"

_comma :: Parser ()
_comma = reserved ","

_openParens :: Parser ()
_openParens = reserved "("

_closeParens :: Parser ()
_closeParens = reserved ")"

_colon :: Parser ()
_colon = reserved ":"

_at :: Parser ()
_at = reserved "@"

_combine :: Parser ()
_combine = do
    void (Text.Parser.Char.char '∧') <|> void (Text.Parser.Char.text "/\\")
    whitespace

_prefer :: Parser ()
_prefer = do
    void (Text.Parser.Char.char '⫽') <|> void (Text.Parser.Char.text "//")
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
    void (Text.Parser.Char.char '∀') <|> void (Text.Parser.Char.text "forall")
    whitespace

_arrow :: Parser ()
_arrow = do
    void (Text.Parser.Char.char '→') <|> void (Text.Parser.Char.text "->")
    whitespace

-- TODO: Follow grammar
doubleLiteral :: Parser Double
doubleLiteral = do
    sign <-  fmap (\_ -> negate) (Text.Parser.Char.char '-')
         <|> pure id
    a    <-  Text.Parser.Token.double
    return (sign a)

-- TODO: Follow grammar
integerLiteral :: Parser Integer
integerLiteral = Text.Parser.Token.integer

-- TODO: Follow grammar
naturalLiteral :: Parser Natural
naturalLiteral = do
    _ <- Text.Parser.Char.char '+'
    a <- Text.Parser.Token.natural
    return (fromIntegral a)

identifier :: Parser Var
identifier = do
    x <- label

    let indexed = do
            _ <- Text.Parser.Char.char '@'
            Text.Parser.Token.natural

    n <- indexed <|> pure 0
    return (V x n)

headPathCharacter :: Char -> Bool
headPathCharacter c =
        ('\x21' <= c && c <= '\x27')
    ||  ('\x2A' <= c && c <= '\x2E')
    ||  ('\x30' <= c && c <= '\x3B')
    ||  c == '\x3D'
    ||  ('\x3F' <= c && c <= '\x5A')
    ||  ('\x5E' <= c && c <= '\x7A')
    ||  ('\x7C' <= c && c <= '\x7E')

pathCharacter :: Char -> Bool
pathCharacter c =
        headPathCharacter c
    ||  c == '\\'
    ||  c == '/'

fileRaw :: Parser PathType
fileRaw = try absolutePath <|> relativePath <|> parentPath <|> homePath
  where
    absolutePath = do
        _  <- Text.Parser.Char.char '/'
        a  <- Text.Parser.Char.satisfy headPathCharacter
        bs <- many (Text.Parser.Char.satisfy pathCharacter)
        let string = '/':a:bs
        return (File Homeless (Filesystem.Path.CurrentOS.decodeString string))

    relativePath = do
        _  <- Text.Parser.Char.text "./"
        as <- many (Text.Parser.Char.satisfy pathCharacter)
        let string = "./" <> as
        return (File Homeless (Filesystem.Path.CurrentOS.decodeString string))

    parentPath = do
        _  <- Text.Parser.Char.text "../"
        as <- many (Text.Parser.Char.satisfy pathCharacter)
        let string = "../" <> as
        return (File Homeless (Filesystem.Path.CurrentOS.decodeString string))

    homePath = do
        _  <- Text.Parser.Char.text "~/"
        as <- many (Text.Parser.Char.satisfy pathCharacter)
        return (File Home (Filesystem.Path.CurrentOS.decodeString as))

file :: Parser PathType
file = do
    a <- fileRaw
    whitespace
    return a

scheme :: Parser Builder
scheme = "http" <> option "s"

httpRaw :: Parser Builder
httpRaw =
        scheme
    <>  "://"
    <>  authority
    <>  pathAbempty
    <>  option ("?" <> query)
    <>  option ("#" <> fragment)

authority :: Parser Builder
authority = option (try (userinfo <> "@")) <> host <> option (":" <> port)

userinfo :: Parser Builder
userinfo = star (satisfy predicate <|> pctEncoded)
  where
    predicate c = unreserved c || subDelims c || c == ':'

host :: Parser Builder
host = ipLiteral <|> ipV4Address <|> regName

port :: Parser Builder
port = star (satisfy digit)

ipLiteral :: Parser Builder
ipLiteral = "[" <> (ipV6Address <|> ipVFuture) <> "]"

ipVFuture :: Parser Builder
ipVFuture = "v" <> plus (satisfy hexdig) <> "." <> plus (satisfy predicate)
  where
    predicate c = unreserved c || subDelims c || c == ':'

ipV6Address :: Parser Builder
ipV6Address = do
        alternative0
    <|> alternative1
    <|> alternative2
    <|> alternative3
    <|> alternative4
    <|> alternative5
    <|> alternative6
    <|> alternative7
    <|> alternative8
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

h16 :: Parser Builder
h16 = range 1 3 (satisfy hexdig)

ls32 :: Parser Builder
ls32 = (h16 <> ":" <> h16) <|> ipV4Address

ipV4Address :: Parser Builder
ipV4Address = decOctet <> "." <> decOctet <> "." <> decOctet <> "." <> decOctet

decOctet :: Parser Builder
decOctet =
        alternative0
    <|> alternative1
    <|> alternative2
    <|> alternative3
    <|> alternative4
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

regName :: Parser Builder
regName = star (satisfy predicate <|> pctEncoded)
  where
    predicate c = unreserved c || subDelims c

pathAbempty :: Parser Builder
pathAbempty = star ("/" <> segment)

segment :: Parser Builder
segment = star pchar

pchar :: Parser Builder
pchar = satisfy predicate <|> pctEncoded
  where
    predicate c = unreserved c || subDelims c || c == ':' || c == '@'

query :: Parser Builder
query = star (pchar <|> satisfy predicate)
  where
    predicate c = c == '/' || c == '?'

fragment :: Parser Builder
fragment = star (pchar <|> satisfy predicate)
  where
    predicate c = c == '/' || c == '?'

pctEncoded :: Parser Builder
pctEncoded = "%" <> count 2 (satisfy hexdig)

unreserved :: Char -> Bool
unreserved c =
    alpha c || digit c || c == '-' || c == '.' || c == '_' || c == '~'

subDelims :: Char -> Bool
subDelims c = c `elem` ("!$&\\()*+,:=" :: String)

http :: Parser PathType
http = do
    a <- httpRaw
    whitespace
    b <- optional (do
        _using
        pathType_ )
    return (URL (Data.Text.Lazy.Builder.toLazyText a) b)

env :: Parser PathType
env = do
    _ <- Text.Parser.Char.text "env:"
    a <- (alternative0 <|> alternative1)
    whitespace
    return (Env a)
  where
    alternative0 = do
        a <- bashEnvironmentVariable
        return (Data.Text.Lazy.Builder.toLazyText a)

    alternative1 = do
        _ <- Text.Parser.Char.char '"'
        a <- posixEnvironmentVariable
        _ <- Text.Parser.Char.char '"'
        return (Data.Text.Lazy.Builder.toLazyText a)

bashEnvironmentVariable :: Parser Builder
bashEnvironmentVariable = satisfy predicate0 <> star (satisfy predicate1)
  where
    predicate0 c = alpha c || c == '_'

    predicate1 c = alpha c || digit c || c == '_'

posixEnvironmentVariable :: Parser Builder
posixEnvironmentVariable = plus posixEnvironmentVariableCharacter

posixEnvironmentVariableCharacter :: Parser Builder
posixEnvironmentVariableCharacter =
    ("\\" <> satisfy predicate0) <|> satisfy predicate1
  where
    predicate0 c = c `elem` ("\"\\abfnrtv" :: String)

    predicate1 c =
            ('\x20' <= c && c <= '\x21')
        ||  ('\x23' <= c && c <= '\x3C')
        ||  ('\x3E' <= c && c <= '\x5B')
        ||  ('\x5D' <= c && c <= '\x7E')

expression :: Parser a -> Parser (Expr Src a)
expression embedded =
        alternative0
    <|> alternative1
    <|> alternative2
    <|> alternative3
    <|> alternative4
    <|> alternative5
  where
    alternative0 = do
        _lambda
        _openParens
        a <- label
        _colon
        b <- expression embedded
        _closeParens
        _arrow
        c <- expression embedded
        return (Lam a b c)

    alternative1 = do
        _if
        a <- expression embedded
        _then
        b <- expression embedded
        _else
        c <- expression embedded
        return (BoolIf a b c)

    alternative2 = do
        _let
        a <- label
        b <- optional (do
            _colon
            expression embedded )
        _equal
        c <- expression embedded
        _in
        d <- expression embedded
        return (Let a b c d)

    alternative3 = do
        _forall
        _openParens
        a <- label
        _colon
        b <- expression embedded
        _closeParens
        _arrow
        c <- expression embedded
        return (Pi a b c)

    alternative4 = do
        a <- try (do a <- operatorExpression embedded; _arrow; return a)
        b <- expression embedded
        return (Pi "_" a b)

    alternative5 = annotatedExpression embedded

annotatedExpression :: Parser a -> Parser (Expr Src a)
annotatedExpression embedded =
    alternative0 <|> try alternative1 <|> alternative2
  where
    alternative0 = do
        _merge
        a <- selectorExpression embedded
        b <- selectorExpression embedded
        c <- optional (do
            _colon
            applicationExpression embedded )
        return (Merge a b c)

    alternative1 = do
        _openBracket
        (emptyCollection embedded <|> nonEmptyOptional embedded)

    alternative2 = do
        a <- operatorExpression embedded
        b <- optional (do _colon; expression embedded)
        case b of
            Nothing -> return a
            Just c  -> return (Annot a c)

emptyCollection :: Parser a -> Parser (Expr Src a)
emptyCollection embedded = do
    _closeBracket
    _colon
    a <- alternative0 <|> alternative1
    b <- selectorExpression embedded
    return (a b empty)
  where
    alternative0 = do
        _List
        return (\a b -> ListLit (Just a) b)

    alternative1 = do
        _Optional
        return OptionalLit

nonEmptyOptional :: Parser a -> Parser (Expr Src a)
nonEmptyOptional embedded = do
    a <- expression embedded
    _closeBracket
    _colon
    _Optional
    b <- selectorExpression embedded
    return (OptionalLit b (pure a))

operatorExpression :: Parser a -> Parser (Expr Src a)
operatorExpression = orExpression

makeOperatorExpression
    :: (Parser a -> Parser (Expr Src a))
    -> Parser ()
    -> (Expr Src a -> Expr Src a -> Expr Src a)
    -> Parser a
    -> Parser (Expr Src a)
makeOperatorExpression subExpression operatorParser operator embedded = do
    a <- subExpression embedded
    b <- many (do operatorParser; subExpression embedded)
    return (foldr1 operator (a:b))

orExpression :: Parser a -> Parser (Expr Src a)
orExpression =
    makeOperatorExpression plusExpression _or BoolOr

plusExpression :: Parser a -> Parser (Expr Src a)
plusExpression =
    makeOperatorExpression textAppendExpression _plus NaturalPlus

textAppendExpression :: Parser a -> Parser (Expr Src a)
textAppendExpression =
    makeOperatorExpression listAppendExpression _textAppend TextAppend

listAppendExpression :: Parser a -> Parser (Expr Src a)
listAppendExpression =
    makeOperatorExpression andExpression _listAppend ListAppend

andExpression :: Parser a -> Parser (Expr Src a)
andExpression =
    makeOperatorExpression combineExpression _and BoolAnd

combineExpression :: Parser a -> Parser (Expr Src a)
combineExpression =
    makeOperatorExpression preferExpression _combine Combine

preferExpression :: Parser a -> Parser (Expr Src a)
preferExpression =
    makeOperatorExpression timesExpression _prefer Prefer

timesExpression :: Parser a -> Parser (Expr Src a)
timesExpression =
    makeOperatorExpression equalExpression _times NaturalTimes

equalExpression :: Parser a -> Parser (Expr Src a)
equalExpression =
    makeOperatorExpression notEqualExpression _doubleEqual BoolEQ

notEqualExpression :: Parser a -> Parser (Expr Src a)
notEqualExpression =
    makeOperatorExpression applicationExpression _notEqual BoolNE

applicationExpression :: Parser a -> Parser (Expr Src a)
applicationExpression embedded = do
    a <- selectorExpression embedded
    b <- many (try (selectorExpression embedded))
    return (foldl1 App (a:b))

selectorExpression :: Parser a -> Parser (Expr Src a)
selectorExpression embedded = do
    a <- primitiveExpression embedded
    b <- many (try (do _dot; label))
    return (foldl Field a b)

primitiveExpression :: Parser a -> Parser (Expr Src a)
primitiveExpression embedded =
        alternative00
    <|> alternative01
    <|> alternative02
    <|> alternative03
    <|> alternative04
    <|> alternative05
    <|> alternative06
    <|> alternative07
    <|> alternative08
    <|> alternative09
    <|> alternative10
    <|> alternative11
    <|> alternative12
    <|> alternative13
    <|> alternative14
    <|> alternative15
    <|> alternative16
    <|> alternative17
    <|> alternative18
    <|> alternative19
    <|> alternative20
    <|> alternative21
    <|> alternative22
    <|> alternative23
    <|> alternative24
    <|> alternative25
    <|> alternative26
    <|> alternative27
    <|> alternative28
    <|> alternative29
    <|> alternative30
    <|> alternative31
    <|> alternative32
    <|> alternative33
    <|> alternative34
    <|> alternative35
    <|> alternative36
    <|> alternative37
    <|> alternative38
  where
    alternative00 = do
        a <- try doubleLiteral
        return (DoubleLit a)

    alternative01 = do
        a <- try naturalLiteral
        return (NaturalLit a)

    alternative02 = do
        a <- try integerLiteral
        return (IntegerLit a)

    alternative03 = textLiteral embedded

    alternative04 = do
        _openBrace
        a <- recordTypeOrLiteral embedded
        _closeBrace
        return a

    alternative05 = do
        _openAngle
        a <- unionTypeOrLiteral embedded
        _closeAngle
        return a

    alternative06 = nonEmptyListLiteral embedded

    alternative07 = do
        a <- embedded
        return (Embed a)

    alternative08 = do
        _NaturalFold
        return NaturalFold

    alternative09 = do
        _NaturalBuild
        return NaturalBuild

    alternative10 = do
        _NaturalIsZero
        return NaturalIsZero

    alternative11 = do
        _NaturalEven
        return NaturalEven

    alternative12 = do
        _NaturalOdd
        return NaturalOdd

    alternative13 = do
        _NaturalToInteger
        return NaturalToInteger

    alternative14 = do
        _NaturalShow
        return NaturalShow

    alternative15 = do
        _IntegerShow
        return IntegerShow

    alternative16 = do
        _DoubleShow
        return DoubleShow

    alternative17 = do
        _ListBuild
        return ListBuild

    alternative18 = do
        _ListFold
        return ListFold

    alternative19 = do
        _ListLength
        return ListLength

    alternative20 = do
        _ListHead
        return ListHead

    alternative21 = do
        _ListLast
        return ListLast

    alternative22 = do
        _ListIndexed
        return ListIndexed

    alternative23 = do
        _ListReverse
        return ListReverse

    alternative24 = do
        _OptionalFold
        return OptionalFold

    alternative25 = do
        _OptionalBuild
        return OptionalBuild

    alternative26 = do
        _Bool
        return Bool

    alternative27 = do
        _Optional
        return Optional

    alternative28 = do
        _Natural
        return Natural

    alternative29 = do
        _Integer
        return Integer

    alternative30 = do
        _Double
        return Double

    alternative31 = do
        _Text
        return Text

    alternative32 = do
        _List
        return List

    alternative33 = do
        _True
        return (BoolLit True)

    alternative34 = do
        _False
        return (BoolLit False)

    alternative35 = do
        _Type
        return (Const Type)

    alternative36 = do
        _Kind
        return (Const Kind)

    alternative37 = do
        a <- identifier
        return (Var a)

    alternative38 = do
        _openParens
        a <- expression embedded
        _closeParens
        return a

recordTypeOrLiteral :: Parser a -> Parser (Expr Src a)
recordTypeOrLiteral embedded =
        alternative0
    <|> alternative1
    <|> alternative2
  where
    alternative0 = do
        _equal
        return (RecordLit Data.Map.empty)

    alternative1 = nonEmptyRecordTypeOrLiteral embedded

    alternative2 = return (Record Data.Map.empty)

nonEmptyRecordTypeOrLiteral :: Parser a -> Parser (Expr Src a)
nonEmptyRecordTypeOrLiteral embedded = do
    a <- label

    let nonEmptyRecordType = do
            _colon
            b <- expression embedded
            e <- many (do
                _comma
                c <- label
                _colon
                d <- expression embedded
                return (c, d) )
            return (Record (Data.Map.fromList ((a, b):e)))

    let nonEmptyRecordLiteral = do
            _equal
            b <- expression embedded
            e <- many (do
                _comma
                c <- label
                _equal
                d <- expression embedded
                return (c, d) )
            return (RecordLit (Data.Map.fromList ((a, b):e)))

    nonEmptyRecordType <|> nonEmptyRecordLiteral

unionTypeOrLiteral :: Parser a -> Parser (Expr Src a)
unionTypeOrLiteral embedded =
    nonEmptyUnionTypeOrLiteral embedded <|> return (Union Data.Map.empty)

nonEmptyUnionTypeOrLiteral :: Parser a -> Parser (Expr Src a)
nonEmptyUnionTypeOrLiteral embedded = do
    (f, kvs) <- loop
    m <- toMap kvs
    return (f m)
  where
    loop = do
        a <- label

        let alternative0 = do
                _equal
                b <- expression embedded
                kvs <- many (do
                    _bar
                    c <- label
                    _colon
                    d <- expression embedded
                    return (c, d) )
                return (UnionLit a b, kvs)

        let alternative1 = do
                _colon
                b <- expression embedded

                let alternative2 = do
                        _bar
                        (f, kvs) <- loop
                        return (f, (a, b):kvs)

                let alternative3 = return (Union, [(a, b)])

                alternative2 <|> alternative3

        alternative0 <|> alternative1

nonEmptyListLiteral :: Parser a -> Parser (Expr Src a)
nonEmptyListLiteral embedded = do
    _openBracket
    a <- expression embedded
    b <- many (do _comma; expression embedded)
    _closeBracket
    return (ListLit Nothing (Data.Vector.fromList (a:b)))

completeExpression :: Parser a -> Parser (Expr Src a)
completeExpression embedded = do
    whitespace
    expression embedded

-- TODO: Mirror `try`s

--------

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

-- | Parser for a top-level Dhall expression
expr :: Parser (Expr Src Path)
expr = exprA import_

-- | Parser for a top-level Dhall expression. The expression is parameterized
-- over any parseable type, allowing the language to be extended as needed.
exprA :: Parser a -> Parser (Expr Src a)
exprA = expression

pathType_ :: Parser PathType
pathType_ = file <|> http <|> env

import_ :: Parser Path
import_ = do
    pathType <- pathType_
    pathMode <- alternative <|> pure Code
    return (Path {..})
  where
    alternative = do
        _as
        _Text
        return RawText

-- | A parsing error
newtype ParseError = ParseError Doc deriving (Typeable)

instance Show ParseError where
    show (ParseError doc) =
      "\n\ESC[1;31mError\ESC[0m: Invalid input\n\n" <> show doc

instance Exception ParseError

-- | Parse an expression from `Text` containing a Dhall program
exprFromText :: Delta -> Text -> Either ParseError (Expr Src Path)
exprFromText delta text = fmap snd (exprAndHeaderFromText delta text)

{-| Like `exprFromText` but also returns the leading comments and whitespace
    (i.e. header) up to the last newline before the code begins

    In other words, if you have a Dhall file of the form:

> -- Comment 1
> {- Comment -} 2

    Then this will preserve @Comment 1@, but not @Comment 2@

    This is used by @dhall-format@ to preserve leading comments and whitespace
-}
exprAndHeaderFromText
    :: Delta
    -> Text
    -> Either ParseError (Text, Expr Src Path)
exprAndHeaderFromText delta text = case result of
    Failure errInfo    -> Left (ParseError (Text.Trifecta._errDoc errInfo))
    Success (bytes, r) -> case Data.Text.Encoding.decodeUtf8' bytes of
        Left  errInfo -> Left (ParseError (fromString (show errInfo)))
        Right txt     -> do
            let stripped = Data.Text.dropWhileEnd (/= '\n') txt
            let lazyText = Data.Text.Lazy.fromStrict stripped
            Right (lazyText, r)
  where
    string = Data.Text.Lazy.unpack text

    parser = unParser (do
        bytes <- Text.Trifecta.slicedWith (\_ x -> x) whitespace
        r <- expr
        Text.Parser.Combinators.eof
        return (bytes, r) )

    result = Text.Trifecta.parseString parser delta string
