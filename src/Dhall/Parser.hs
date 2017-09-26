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
import Data.CharSet (CharSet)
import Data.Functor (void)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Sequence (ViewL(..))
import Data.String (IsString(..))
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
import qualified Data.CharSet
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
    fromString x = pure (fromString x)

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

char :: Char -> Parser Builder
char c = fmap Data.Text.Lazy.Builder.singleton (Text.Parser.Char.char c)

range :: Monoid a => Int -> Int -> Parser a -> Parser a
range minBound maxMatches parser = do
    xs <- replicateM minBound parser
    ys <- loop maxMatches
    return (mconcat xs <> ys)
  where
    loop 0 = return mempty
    loop n =
            (do x <- parser; xs <- loop (n - 1); return (x <> xs))
        <|> return mempty

count :: Monoid a => Int -> Parser a -> Parser a
count n parser = fmap mconcat (replicateM 3 parser)

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
            ('\x20' <= c && c <= '\x7A')
        ||  c == '\x7C'
        ||  ('\x7E' <= c && c <= '\x10FFFF')
        ||  c == '\n'
        ||  c == '\t'

blockComment :: Parser ()
blockComment = do
    _ <- Text.Parser.Char.char '{'
    Text.Parser.Combinators.skipMany notBrace
    Text.Parser.Combinators.skipMany $ do
        blockComment
        Text.Parser.Combinators.skipMany notBrace
    _ <- Text.Parser.Char.char '}'
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
simpleLabel = do
    c  <- Text.Parser.Char.satisfy headCharacter
    cs <- many (Text.Parser.Char.satisfy tailCharacter)
    return (Data.Text.Lazy.pack (c:cs))
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
    <|> endOfLine
    <|> tab
  where
        escapeSingleQuotes = do
            _ <- Text.Parser.Char.text "'''"
            return (TextLit "'''")

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
            predicate c = '\x20' <= c && c <= '\x10FFFF'

        endOfLine =
                (do _ <- Text.Parser.Char.char '\n'  ; return (TextLit "\n"  ))
            <|> (do _ <- Text.Parser.Char.text "\r\n"; return (TextLit "\r\n"))

        tab = do
            _ <- Text.Parser.Char.char '\t'
            return (TextLit "\t")

singleQuotedLiteral :: Parser a -> Parser (Expr Src a)
singleQuotedLiteral embedded = do
    _      <- Text.Parser.Char.text "''"
    chunks <- many (singleQuotedChunk embedded)
    _      <- Text.Parser.Char.text "''"
    return (foldr textAppend (TextLit "") chunks)

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
naturalLiteral :: Parser Integer
naturalLiteral = do
    _ <- Text.Parser.Char.char '+'
    Text.Parser.Token.natural

expression :: a
expression = undefined

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
        return (File Homeless (Filesystem.Path.CurrentOS.decodeString (a:bs)))

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

unreserved :: Char -> Bool
unreserved c =
    alpha c || digit c || c == '-' || c == '.' || c == '_' || c == '~'

pctEncoded :: Parser Builder
pctEncoded = do
    _ <- Text.Parser.Char.char '%'
    a <- Text.Parser.Char.satisfy hexdig
    b <- Text.Parser.Char.satisfy hexdig
    return (Data.Text.Lazy.Builder.fromString ['%', a, b])

subDelims :: Char -> Bool
subDelims c =
        c == '!'
    ||  c == '$'
    ||  c == '&'
    ||  c == '\''
    ||  c == '('
    ||  c == ')'
    ||  c == '*'
    ||  c == '+'
    ||  c == ','
    ||  c == ';'
    ||  c == '='

userinfo :: Parser Builder
userinfo = do
    let predicate c = unreserved c || subDelims c || c == ':'

    let parseChar = do
            c <- Text.Parser.Char.satisfy predicate
            return (Data.Text.Lazy.Builder.singleton c)

    cs <- many (parseChar <|> pctEncoded)
    return (mconcat cs)

h16 :: Parser Builder
h16 = range 1 3 (Text.Parser.Char.satisfy hexdig)

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

ipV4Address :: Parser Builder
ipV4Address = decOctet <> "." <> decOctet <> "." <> decOctet <> "." <> decOctet

ls32 :: Parser Builder
ls32 = (h16 <> ":" <> h16) <|> ipV4Address

ipV6Address :: Parser Builder
ipV6Address = do
        alternative0
    <|> alternative1
  where
    alternative0 = count 6 (h16 <> ":") <> ls32

    alternative1 = "::" <> count 5 (h16 <> ":") <> ls32

    alternative2 = (h16 <|> "") <> "::" <> count 4 (h16 <> ":") <> ls32

    alternative3 =
            ((range 0 1 (h16 <> ":") <> h16) <|> "")
        <>  "::"
        <>  count 3 (h16 <> ":")
        <>  ls32

    alternative4 =
        

pchar :: Parser Builder
pchar = (do c <- Text.Parser.Char.satisfy unreserved
            return (Data.Text.Lazy.Builder.singleton c)
        )
    <|> pctEncoded
    <|> (do c <- Text.Parser.Char.satisfy subDelims
            return (Data.Text.Lazy.Builder.singleton c)
        )
    <|> char ':'
    <|> char '@'

scheme :: Parser ()
scheme =
    void (Text.Parser.Char.text "https") <|> void (Text.Parser.Char.text "http")

httpRaw :: Parser PathType
httpRaw = do
    scheme
    Text.Parser.Char.text "://"
    authority

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

stringLiteral :: Show a => Parser a -> Parser (Expr Src a)
stringLiteral embedded =
    Text.Parser.Token.token
        (   doubleQuoteLiteral embedded
        <|> doubleSingleQuoteString embedded
        )

doubleQuoteLiteral :: Show a => Parser a -> Parser (Expr Src a)
doubleQuoteLiteral embedded = do
    _  <- Text.Parser.Char.char '"'
    go
  where
    go = go0 <|> go1 <|> go2 <|> go3

    go0 = do
        _ <- Text.Parser.Char.char '"'
        return (TextLit mempty) 

    go1 = do
        _ <- Text.Parser.Char.text "${"
        Text.Parser.Token.whiteSpace
        a <- exprA embedded
        _ <- Text.Parser.Char.char '}'
        b <- go
        return (TextAppend a b)

    go2 = do
        _ <- Text.Parser.Char.text "''${"
        b <- go
        let e = case b of
                TextLit cs ->
                    TextLit ("${" <> cs)
                TextAppend (TextLit cs) d ->
                    TextAppend (TextLit ("${" <> cs)) d
                _ ->
                    TextAppend (TextLit "${") b
        return e

    go3 = do
        a <- stringChar
        b <- go
        let e = case b of
                TextLit cs ->
                    TextLit (build a <> cs)
                TextAppend (TextLit cs) d ->
                    TextAppend (TextLit (build a <> cs)) d
                _ ->
                    TextAppend (TextLit (build a)) b
        return e

doubleSingleQuoteString :: Show a => Parser a -> Parser (Expr Src a)
doubleSingleQuoteString embedded = do
    expr0 <- p0

    let builder0      = concatFragments expr0
    let text0         = Data.Text.Lazy.Builder.toLazyText builder0
    let lines0        = Data.Text.Lazy.lines text0
    let isEmpty       = Data.Text.Lazy.all Data.Char.isSpace
    let nonEmptyLines = filter (not . isEmpty) lines0

    let indentLength line =
            Data.Text.Lazy.length
                (Data.Text.Lazy.takeWhile Data.Char.isSpace line)

    let shortestIndent = case nonEmptyLines of
            [] -> 0
            _  -> minimum (map indentLength nonEmptyLines)

    -- The purpose of this complicated `trim0`/`trim1` is to ensure that we
    -- strip leading whitespace without stripping whitespace after variable
    -- interpolation
    let trim0 =
              build
            . Data.Text.Lazy.intercalate "\n"
            . map (Data.Text.Lazy.drop shortestIndent)
            . Data.Text.Lazy.splitOn "\n"
            . Data.Text.Lazy.Builder.toLazyText

    let trim1 builder = build (Data.Text.Lazy.intercalate "\n" lines_)
          where
            text = Data.Text.Lazy.Builder.toLazyText builder

            lines_ = case Data.Text.Lazy.splitOn "\n" text of
                []   -> []
                l:ls -> l:map (Data.Text.Lazy.drop shortestIndent) ls

    let process trim (TextAppend (TextLit t) e) =
            TextAppend (TextLit (trim t)) (process trim1 e)
        process _    (TextAppend e0 e1) =
            TextAppend e0 (process trim1 e1)
        process trim (TextLit t) =
            TextLit (trim t)
        process _     e =
            e

    return (process trim0 expr0)
  where
    -- This treats variable interpolation as breaking leading whitespace for the
    -- purposes of computing the shortest leading whitespace.  The "${VAR}"
    -- could really be any text that breaks whitespace
    concatFragments (TextAppend (TextLit t) e) = t        <> concatFragments e
    concatFragments (TextAppend  _          e) = "${VAR}" <> concatFragments e
    concatFragments (TextLit t)                = t
    concatFragments  _                         = mempty

    p0 = do
        _ <- Text.Parser.Char.string "''"
        _ <- optional (Text.Parser.Char.char '\n')
        p1

    p1 =    p2
        <|> p3
        <|> p4
        <|> p5 (Text.Parser.Char.char '\'')
        <|> p6
        <|> p5 Text.Parser.Char.anyChar

    p2 = do
        _  <- Text.Parser.Char.text "'''"
        s1 <- p1
        let s4 = case s1 of
                TextLit s2 ->
                    TextLit ("''" <> s2)
                TextAppend (TextLit s2) s3 ->
                    TextAppend (TextLit ("''" <> s2)) s3
                _ ->
                    TextAppend (TextLit "''") s1
        return s4

    p3 = do
        _  <- Text.Parser.Char.text "''${"
        s1 <- p1
        let s4 = case s1 of
                TextLit s2 ->
                    TextLit ("${" <> s2)
                TextAppend (TextLit s2) s3 ->
                    TextAppend (TextLit ("${" <> s2)) s3
                _ ->
                    TextAppend (TextLit "${") s1
        return s4

    p4 = do
        _ <- Text.Parser.Char.text "''"
        return (TextLit mempty)

    p5 parser = do
        s0 <- parser
        s1 <- p1
        let s4 = case s1 of
                TextLit s2 ->
                    TextLit (build s0 <> s2)
                TextAppend (TextLit s2) s3 ->
                    TextAppend (TextLit (build s0 <> s2)) s3
                _ -> TextAppend (TextLit (build s0)) s1
        return s4

    p6 = do
        _  <- Text.Parser.Char.text "${"
        Text.Parser.Token.whiteSpace
        s1 <- exprA embedded
        _  <- Text.Parser.Char.char '}'
        s3 <- p1
        return (TextAppend s1 s3)

stringChar :: Parser Char
stringChar =
        Text.Parser.Char.satisfy predicate
    <|> (do _ <- Text.Parser.Char.text "\\\\"; return '\\')
    <|> (do _ <- Text.Parser.Char.text "\\\""; return '"' )
    <|> (do _ <- Text.Parser.Char.text "\\n" ; return '\n')
    <|> (do _ <- Text.Parser.Char.text "\\r" ; return '\r')
    <|> (do _ <- Text.Parser.Char.text "\\t" ; return '\t')
  where
    predicate c = c /= '"' && c /= '\\' && c > '\026'

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

{-
label :: Parser Text
label = (normalIdentifier <|> escapedIdentifier) <?> "label"
  where
    normalIdentifier = Text.Parser.Token.ident identifierStyle

    escapedIdentifier = Text.Parser.Token.token (do
        _  <- Text.Parser.Char.char '`'
        c  <- Text.Parser.Token._styleStart  identifierStyle
        cs <- many (Text.Parser.Token._styleLetter identifierStyle)
        _  <- Text.Parser.Char.char '`'
        return (Data.Text.Lazy.pack (c:cs)) )
-}

-- | Parser for a top-level Dhall expression
expr :: Parser (Expr Src Path)
expr = exprA import_

-- | Parser for a top-level Dhall expression. The expression is parameterized
-- over any parseable type, allowing the language to be extended as needed.
exprA :: Show a => Parser a -> Parser (Expr Src a)
exprA embedded =
    (   noted
        (   choice
            [       exprA0
            ,       exprA1
            ,       exprA2
            ,       exprA3
            ,       exprA4
            ]
        )
    )   <|> exprA5
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
        a <- try (exprC embedded <* arrow)
        b <- exprA embedded
        return (Pi "_" a b)

    exprA5 = exprB embedded

exprB :: Show a => Parser a -> Parser (Expr Src a)
exprB embedded =
    noted
    (   choice
        [       exprB0
        ,   try exprB1
        ,       exprB2
        ]
    )
  where
    exprB0 = do
        reserve "merge"
        a <- exprE embedded
        b <- exprE embedded
        c <- optional (do
            symbol ":"
            exprD embedded )
        return (Merge a b c)

    exprB1 = do
        symbol "["

        let emptyListOrOptional = do
                symbol "]"
                symbol ":"

                let emptyList = do
                        reserve "List"
                        a <- exprE embedded
                        return (ListLit (Just a) Data.Vector.empty)

                let emptyOptional = do
                        reserve "Optional"
                        a <- exprE embedded
                        return (OptionalLit a Data.Vector.empty)

                emptyList <|> emptyOptional

        let nonEmptyOptional = do
                a <- exprA embedded
                symbol "]"
                symbol ":"
                reserve "Optional"
                b <- exprE embedded
                return (OptionalLit b (Data.Vector.singleton a))

        emptyListOrOptional <|> nonEmptyOptional

    exprB2 = do
        a <- exprC embedded

        let exprB2A = do
                symbol ":"
                b <- exprA embedded
                return (Annot a b)

        let exprB2B = pure a

        exprB2A <|> exprB2B

exprC :: Show a => Parser a -> Parser (Expr Src a)
exprC embedded = exprC0
  where
    chain pA pOp op pB = noted (do
        a <- pA
        (do _ <- pOp <?> "operator"; b <- pB; return (op a b)) <|> pure a )

    exprC0 = chain  exprC1          (symbol "||") BoolOr       exprC0
    exprC1 = chain  exprC2          (symbol "+" ) NaturalPlus  exprC1
    exprC2 = chain  exprC3          (symbol "++") TextAppend   exprC2
    exprC3 = chain  exprC4          (symbol "#" ) ListAppend   exprC3
    exprC4 = chain  exprC5          (symbol "&&") BoolAnd      exprC4
    exprC5 = chain  exprC6           combine      Combine      exprC5
    exprC6 = chain  exprC7           prefer       Prefer       exprC6
    exprC7 = chain  exprC8          (symbol "*" ) NaturalTimes exprC7
    exprC8 = chain  exprC9          (symbol "==") BoolEQ       exprC8
    exprC9 = chain (exprD embedded) (symbol "!=") BoolNE       exprC9

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
    es <- some (noted (exprE embedded))
    let app nL@(Note (Src before _ bytesL) _) nR@(Note (Src _ after bytesR) _) =
            Note (Src before after (bytesL <> bytesR)) (App nL nR)
        app nL nR = App nL nR
    return (Data.List.foldl1 app es)

exprE :: Show a => Parser a -> Parser (Expr Src a)
exprE embedded = noted (do
    a <- exprF embedded

    let field = try (do
            symbol "."
            label )

    b <- many field

    return (Data.List.foldl Field a b) )

exprF :: Show a => Parser a -> Parser (Expr Src a)
exprF embedded =
    noted
    (   choice
        [   try exprParseDouble
        ,   try exprNaturalLit
        ,   try exprIntegerLit
        ,       exprStringLiteral
        ,       exprRecordTypeOrLiteral
        ,       exprUnionTypeOrLiteral
        ,       exprListLiteral
        ,       exprImport
        ,   (choice
                [   exprNaturalFold
                ,   exprNaturalBuild
                ,   exprNaturalIsZero
                ,   exprNaturalEven
                ,   exprNaturalOdd
                ,   exprNaturalToInteger
                ,   exprNaturalShow
                ,   exprIntegerShow
                ,   exprDoubleShow
                ,   exprListBuild
                ,   exprListFold
                ,   exprListLength
                ,   exprListHead
                ,   exprListLast
                ,   exprListIndexed
                ,   exprListReverse
                ,   exprOptionalFold
                ,   exprOptionalBuild
                ,   exprBool
                ,   exprOptional
                ,   exprNatural
                ,   exprInteger
                ,   exprDouble
                ,   exprText
                ,   exprList
                ,   exprBoolLitTrue
                ,   exprBoolLitFalse
                ,   exprConst
                ]
            ) <?> "built-in value"
        ,       exprVar
        ]
    )   <|> exprParens
  where
    exprVar = do
        a <- var
        return (Var a)

    exprConst = do
        a <- const
        return (Const a)

    exprNatural = do
        reserve "Natural"
        return Natural

    exprNaturalFold = do
        reserve "Natural/fold"
        return NaturalFold

    exprNaturalBuild = do
        reserve "Natural/build"
        return NaturalBuild

    exprNaturalIsZero = do
        reserve "Natural/isZero"
        return NaturalIsZero

    exprNaturalEven = do
        reserve "Natural/even"
        return NaturalEven

    exprNaturalOdd = do
        reserve "Natural/odd"
        return NaturalOdd

    exprNaturalToInteger = do
        reserve "Natural/toInteger"
        return NaturalToInteger

    exprNaturalShow = do
        reserve "Natural/show"
        return NaturalShow

    exprInteger = do
        reserve "Integer"
        return Integer

    exprIntegerShow = do
        reserve "Integer/show"
        return IntegerShow

    exprDouble = do
        reserve "Double"
        return Double

    exprDoubleShow = do
        reserve "Double/show"
        return DoubleShow

    exprText = do
        reserve "Text"
        return Text

    exprList = do
        reserve "List"
        return List

    exprListBuild = do
        reserve "List/build"
        return ListBuild

    exprListFold = do
        reserve "List/fold"
        return ListFold

    exprListLength = do
        reserve "List/length"
        return ListLength

    exprListHead = do
        reserve "List/head"
        return ListHead

    exprListLast = do
        reserve "List/last"
        return ListLast

    exprListIndexed = do
        reserve "List/indexed"
        return ListIndexed

    exprListReverse = do
        reserve "List/reverse"
        return ListReverse

    exprOptional = do
        reserve "Optional"
        return Optional

    exprOptionalFold = do
        reserve "Optional/fold"
        return OptionalFold

    exprOptionalBuild = do
        reserve "Optional/build"
        return OptionalBuild

    exprBool = do
        reserve "Bool"
        return Bool

    exprBoolLitTrue = do
        reserve "True"
        return (BoolLit True)

    exprBoolLitFalse = do
        reserve "False"
        return (BoolLit False)

    exprIntegerLit = do
        a <- Text.Parser.Token.integer
        return (IntegerLit a)

    exprNaturalLit = (do
        _ <- Text.Parser.Char.char '+'
        a <- Text.Parser.Token.natural
        return (NaturalLit (fromIntegral a)) ) <?> "natural"

    exprParseDouble = do
        sign <-  fmap (\_ -> negate) (Text.Parser.Char.char '-')
             <|> fmap (\_ -> id    ) (Text.Parser.Char.char '+')
             <|> pure id
        a <- Text.Parser.Token.double
        return (DoubleLit (sign a))

    exprStringLiteral = stringLiteral embedded

    exprRecordTypeOrLiteral = recordTypeOrLiteral embedded <?> "record type or literal"

    exprUnionTypeOrLiteral = unionTypeOrLiteral embedded <?> "union type or literal"

    exprListLiteral = listLit embedded <?> "list literal"

    exprImport = do
        a <- embedded <?> "import"
        return (Embed a)

    exprParens = do
        symbol "("
        a <- exprA embedded
        symbol ")"
        return a

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

recordTypeOrLiteral :: Show a => Parser a -> Parser (Expr Src a)
recordTypeOrLiteral embedded = do
    symbol "{"

    let emptyRecordLiteral = do
            symbol "="
            symbol "}"
            return (RecordLit (Data.Map.fromList []))

    let emptyRecordType = do
            symbol "}"
            return (Record (Data.Map.fromList []))

    let nonEmptyRecordTypeOrLiteral = do
            a <- label

            let nonEmptyRecordLiteral = do
                    symbol "="
                    b <- exprA embedded

                    let recordLiteralWithoutOtherFields = do
                            symbol "}"
                            return (RecordLit (Data.Map.singleton a b))

                    let recordLiteralWithOtherFields = do
                            symbol ","
                            c <- fieldValues embedded
                            d <- toMap ((a, b):c)
                            symbol "}"
                            return (RecordLit d)

                    recordLiteralWithoutOtherFields <|> recordLiteralWithOtherFields

            let nonEmptyRecordType = do
                    symbol ":"
                    b <- exprA embedded

                    let recordTypeWithoutOtherFields = do
                            symbol "}"
                            return (Record (Data.Map.singleton a b))

                    let recordTypeWithOtherFields = do
                            symbol ","
                            c <- fieldTypes embedded
                            d <- toMap ((a, b):c)
                            symbol "}"
                            return (Record d)

                    recordTypeWithoutOtherFields <|> recordTypeWithOtherFields

            nonEmptyRecordLiteral <|> nonEmptyRecordType

    emptyRecordLiteral <|> emptyRecordType <|> nonEmptyRecordTypeOrLiteral

fieldValues :: Show a => Parser a -> Parser [(Text, Expr Src a)]
fieldValues embedded = sepBy1 (fieldValue embedded) (symbol ",")

fieldValue :: Show a => Parser a -> Parser (Text, Expr Src a)
fieldValue embedded = do
    a <- label
    symbol "="
    b <- exprA embedded
    return (a, b)

fieldTypes :: Show a => Parser a -> Parser [(Text, Expr Src a)]
fieldTypes embedded = sepBy (fieldType embedded) (symbol ",")

fieldType :: Show a => Parser a -> Parser (Text, Expr Src a)
fieldType embedded = do
    a <- label
    symbol ":"
    b <- exprA embedded
    return (a, b)

unionTypeOrLiteral :: Show a => Parser a -> Parser (Expr Src a)
unionTypeOrLiteral embedded = do
    symbol "<"

    let emptyUnionType = do
            symbol ">"
            return (Union Data.Map.empty)

    let withLabel = do
            a <- label

            let withColon = do
                    symbol ":"
                    b <- exprA embedded

                    let withClose = do
                            symbol ">"
                            return (Union, [(a, b)])

                    let withBar = do
                            symbol "|"

                            let continue = do
                                    (c, d) <- withLabel
                                    return (c, (a, b):d)

                            withClose <|> continue

                    withBar <|> withClose

            let unionLiteral = do
                    symbol "="
                    b <- exprA embedded

                    let emptyUnionLiteral = do
                            symbol ">"
                            return (UnionLit a b, [])

                    let nonEmptyUnionLiteral = do
                            symbol "|"

                            let stop = do
                                    symbol ">"
                                    return (UnionLit a b, [])

                            let continue = do
                                    c <- Text.Parser.Combinators.sepEndBy (alternativeType embedded) (symbol "|")
                                    symbol ">"
                                    return (UnionLit a b, c)

                            stop <|> continue

                    emptyUnionLiteral <|> nonEmptyUnionLiteral

            withColon <|> unionLiteral

    let nonEmptyUnionTypeOrLiteral = do
            (a, b) <- withLabel
            c <- toMap b
            return (a c)

    emptyUnionType <|> nonEmptyUnionTypeOrLiteral

alternativeType :: Show a => Parser a -> Parser (Text, Expr Src a)
alternativeType embedded = do
    a <- label
    symbol ":"
    b <- exprA embedded
    return (a, b)

listLit :: Show a => Parser a -> Parser (Expr Src a)
listLit embedded = do
    symbol "["
    a <- elems embedded
    symbol "]"
    return (ListLit Nothing a)

import_ :: Parser Path
import_ = do
    pathType <- pathType_
    let rawText = do
            _ <- reserve "as"
            _ <- reserve "Text"
            return RawText
    pathMode <- rawText <|> pure Code
    return (Path {..})

pathType_ :: Parser PathType
pathType_ = file <|> url <|> env

pathChar :: Char -> Bool
pathChar c =
    not
    (   Data.Char.isSpace c
    ||  Data.CharSet.member c disallowedPathChars
    )

disallowedPathChars :: CharSet
disallowedPathChars = Data.CharSet.fromList "()[]{}<>"

url :: Parser PathType
url =   try url0
    <|> url1
  where
    url0 = do
        a <- Text.Parser.Char.string "https://"
        b <- many (Text.Parser.Char.satisfy pathChar)
        Text.Parser.Token.whiteSpace
        c <- optional (do
            _ <- Text.Parser.Char.string "using"
            Text.Parser.Token.whiteSpace
            pathType_ )
        return (URL (Data.Text.Lazy.pack (a <> b)) c)

    url1 = do
        a <- Text.Parser.Char.string "http://"
        b <- many (Text.Parser.Char.satisfy pathChar)
        Text.Parser.Token.whiteSpace
        c <- optional (do
            _ <- Text.Parser.Char.string "using"
            Text.Parser.Token.whiteSpace
            pathType_ )
        return (URL (Data.Text.Lazy.pack (a <> b)) c)

env :: Parser PathType
env = do
    _ <- Text.Parser.Char.string "env:"
    a <- many (Text.Parser.Char.satisfy pathChar)
    Text.Parser.Token.whiteSpace
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
