{-# LANGUAGE CPP                        #-}
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
import Data.ByteArray.Encoding (Base(..))
import Data.Functor (void)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Scientific (Scientific)
import Data.Semigroup (Semigroup(..))
import Data.Sequence (ViewL(..))
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Void (Void)
import Dhall.Core
import Formatting.Buildable (Buildable(..))
import Numeric.Natural (Natural)
import Prelude hiding (const, pi)
import Text.Parser.Combinators (choice, try, (<?>))
import Text.Parser.Token (TokenParsing(..))

import qualified Control.Monad
import qualified Crypto.Hash
import qualified Data.ByteArray.Encoding
import qualified Data.ByteString
import qualified Data.Char
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.HashSet
import qualified Data.List
import qualified Data.List.NonEmpty
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Char
import qualified Text.Parser.Char
import qualified Text.Parser.Combinators
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Style

-- | Source code extract
data Src = Src Text.Megaparsec.SourcePos Text.Megaparsec.SourcePos Text
  deriving (Eq, Show)

instance Buildable Src where
    build (Src begin _ text) =
            build text <> "\n"
        <>  "\n"
        <>  build (Text.Megaparsec.sourcePosPretty begin)
        <>  "\n"

{-| A `Parser` that is almost identical to
    @"Text.Megaparsec".`Text.Megaparsec.Parsec`@ except treating Haskell-style
    comments as whitespace
-}
newtype Parser a = Parser { unParser :: Text.Megaparsec.Parsec Void Text a }
    deriving
    (   Functor
    ,   Applicative
    ,   Monad
    ,   Alternative
    ,   MonadPlus
    ,   Text.Megaparsec.MonadParsec Void Text
    )

instance Data.Semigroup.Semigroup a => Data.Semigroup.Semigroup (Parser a) where
    (<>) = liftA2 (<>)

instance (Data.Semigroup.Semigroup a, Monoid a) => Monoid (Parser a) where
    mempty = pure mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance IsString a => IsString (Parser a) where
    fromString x = fromString x <$ Text.Megaparsec.Char.string (fromString x)

instance Text.Parser.Combinators.Parsing Parser where
  try = Text.Megaparsec.try

  (<?>) = (Text.Megaparsec.<?>)

  skipMany = Text.Megaparsec.skipMany

  skipSome = Text.Megaparsec.skipSome

  unexpected = fail

  eof = Parser Text.Megaparsec.eof

  notFollowedBy = Text.Megaparsec.notFollowedBy

instance Text.Parser.Char.CharParsing Parser where
  satisfy = Parser . Text.Megaparsec.Char.satisfy

  char = Text.Megaparsec.Char.char

  notChar = Text.Megaparsec.Char.char

  anyChar = Text.Megaparsec.Char.anyChar

  string = fmap Data.Text.unpack . Text.Megaparsec.Char.string . fromString

  text = Text.Megaparsec.Char.string

instance TokenParsing Parser where
    someSpace =
        Text.Parser.Token.Style.buildSomeSpaceParser
            (Parser (Text.Megaparsec.skipSome (Text.Megaparsec.Char.satisfy Data.Char.isSpace)))
            Text.Parser.Token.Style.haskellCommentStyle

    highlight _ = id

    semi = token (Text.Megaparsec.Char.char ';' <?> ";")

noted :: Parser (Expr Src a) -> Parser (Expr Src a)
noted parser = do
    before      <- Text.Megaparsec.getPosition
    (tokens, e) <- Text.Megaparsec.match parser
    after       <- Text.Megaparsec.getPosition
    return (Note (Src before after tokens) e)

count :: (Semigroup a, Monoid a) => Int -> Parser a -> Parser a
count n parser = mconcat (replicate n parser)

range :: (Semigroup a, Monoid a) => Int -> Int -> Parser a -> Parser a
range minimumBound maximumMatches parser =
    count minimumBound parser <> loop maximumMatches
  where
    loop 0 = mempty
    loop n = (parser <> loop (n - 1)) <|> mempty

option :: (Alternative f, Monoid a) => f a -> f a
option p = p <|> pure mempty

star :: (Alternative f, Monoid a) => f a -> f a
star p = plus p <|> pure mempty

plus :: (Alternative f, Monoid a) => f a -> f a
plus p = mappend <$> p <*> star p

satisfy :: (Char -> Bool) -> Parser Text
satisfy = fmap Data.Text.singleton . Text.Parser.Char.satisfy

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

simpleLabel :: Parser Text
simpleLabel = try (do
    c  <- Text.Parser.Char.satisfy headCharacter
    cs <- many (Text.Parser.Char.satisfy tailCharacter)
    let string = c:cs
    let text = Data.Text.pack string
    Control.Monad.guard (not (Data.HashSet.member text reservedIdentifiers))
    return text )
  where
    headCharacter c = alpha c || c == '_'

    tailCharacter c = alpha c || digit c || c == '_' || c == '-' || c == '/'

backtickLabel :: Parser Text
backtickLabel = do
    _ <- Text.Parser.Char.char '`'
    t <- some (Text.Parser.Char.satisfy predicate)
    _ <- Text.Parser.Char.char '`'
    return (Data.Text.pack t)
  where
    predicate c = alpha c || digit c || elem c ("-/_:." :: String)

label :: Parser Text
label = (do
    t <- backtickLabel <|> simpleLabel
    whitespace
    return t ) <?> "label"

noDuplicates :: Ord a => [a] -> Parser (Set a)
noDuplicates = go Data.Set.empty
  where
    go found    []  = return found
    go found (x:xs) =
        if Data.Set.member x found
        then fail "Duplicate key"
        else go (Data.Set.insert x found) xs

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

doubleQuotedChunk :: Parser a -> Parser (Chunks Src a)
doubleQuotedChunk embedded =
    choice
        [ interpolation
        , unescapedCharacter
        , escapedCharacter
        ]
  where
    interpolation = do
        _ <- Text.Parser.Char.text "${"
        e <- completeExpression embedded
        _ <- Text.Parser.Char.char '}'
        return (Chunks [(mempty, e)] mempty)

    unescapedCharacter = do
        c <- Text.Parser.Char.satisfy predicate
        return (Chunks [] (Data.Text.singleton c))
      where
        predicate c =
                ('\x20' <= c && c <= '\x21'    )
            ||  ('\x23' <= c && c <= '\x5B'    )
            ||  ('\x5D' <= c && c <= '\x10FFFF')

    escapedCharacter = do
        _ <- Text.Parser.Char.char '\\'
        c <- choice
            [ quotationMark
            , dollarSign
            , backSlash
            , forwardSlash
            , backSpace
            , formFeed
            , lineFeed
            , carriageReturn
            , tab
            , unicode
            ]
        return (Chunks [] (Data.Text.singleton c))
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

doubleQuotedLiteral :: Parser a -> Parser (Chunks Src a)
doubleQuotedLiteral embedded = do
    _      <- Text.Parser.Char.char '"'
    chunks <- many (doubleQuotedChunk embedded)
    _      <- Text.Parser.Char.char '"'
    return (mconcat chunks)

-- | Similar to `Dhall.Core.buildChunks` except that this doesn't bother to
-- render interpolated expressions to avoid a `Buildable a` constraint.  The
-- interpolated contents are not necessary for computing how much to dedent a
-- multi-line string
--
-- This also doesn't include the surrounding quotes since they would interfere
-- with the whitespace detection
buildChunks :: Chunks s a -> Text
buildChunks (Chunks a b) = foldMap buildChunk a <> escapeText b
  where
    buildChunk :: (Text, Expr s a) -> Text
    buildChunk (c, _) = escapeText c <> "${x}"

dedent :: Chunks Src a -> Chunks Src a
dedent chunks0 = process chunks0
  where
    text0 = buildChunks chunks0

    lines0 = Data.Text.lines text0

    isEmpty = Data.Text.all Data.Char.isSpace

    nonEmptyLines = filter (not . isEmpty) lines0

    indentLength line =
        Data.Text.length (Data.Text.takeWhile Data.Char.isSpace line)

    shortestIndent = case nonEmptyLines of
        [] -> 0
        _  -> minimum (map indentLength nonEmptyLines)

    -- The purpose of this complicated `trimBegin`/`trimContinue` is to ensure
    -- that we strip leading whitespace without stripping whitespace after
    -- variable interpolation

    -- This is the trim function we use up until the first variable
    -- interpolation, dedenting all lines
    trimBegin =
          Data.Text.intercalate "\n"
        . map (Data.Text.drop shortestIndent)
        . Data.Text.splitOn "\n"

    -- This is the trim function we use after each variable interpolation
    -- where we indent each line except the first line (since it's not a true
    -- beginning of a line)
    trimContinue text = Data.Text.intercalate "\n" lines_
      where
        lines_ = case Data.Text.splitOn "\n" text of
            []   -> []
            l:ls -> l:map (Data.Text.drop shortestIndent) ls

    -- This is the loop that drives whether or not to use `trimBegin` or
    -- `trimContinue`.  We call this function with `trimBegin`, but after the
    -- first interpolation we switch permanently to `trimContinue`
    process (Chunks ((x0, y0):xys) z) =
        Chunks ((trimBegin x0, y0):xys') (trimContinue z)
      where
        xys' = [ (trimContinue x, y) | (x, y) <- xys ]
    process (Chunks [] z) =
        Chunks [] (trimBegin z)

singleQuoteContinue :: Parser a -> Parser (Chunks Src a)
singleQuoteContinue embedded =
    choice
        [ escapeSingleQuotes
        , interpolation
        , escapeInterpolation
        , endLiteral
        , unescapedCharacter
        , tab
        , endOfLine
        ]
  where
        escapeSingleQuotes = do
            _ <- "'''" :: Parser Text
            b <- singleQuoteContinue embedded
            return ("''" <> b)

        interpolation = do
            _ <- Text.Parser.Char.text "${"
            a <- completeExpression embedded
            _ <- Text.Parser.Char.char '}'
            b <- singleQuoteContinue embedded
            return (Chunks [(mempty, a)] mempty <> b)

        escapeInterpolation = do
            _ <- Text.Parser.Char.text "''${"
            b <- singleQuoteContinue embedded
            return ("${" <> b)

        endLiteral = do
            _ <- Text.Parser.Char.text "''"
            return mempty

        unescapedCharacter = do
            a <- satisfy predicate
            b <- singleQuoteContinue embedded
            return (Chunks [] a <> b)
          where
            predicate c = '\x20' <= c && c <= '\x10FFFF'

        endOfLine = do
            a <- "\n" <|> "\r\n"
            b <- singleQuoteContinue embedded
            return (Chunks [] a <> b)

        tab = do
            _ <- Text.Parser.Char.char '\t'
            b <- singleQuoteContinue embedded
            return ("\t" <> b)

singleQuoteLiteral :: Parser a -> Parser (Chunks Src a)
singleQuoteLiteral embedded = do
    _ <- Text.Parser.Char.text "''"

    -- This is technically not in the grammar, but it's still equivalent to the
    -- original grammar and an easy way to discard the first character if it's
    -- a newline
    _ <- optional endOfLine

    a <- singleQuoteContinue embedded

    return (dedent a)
  where
    endOfLine =
            void (Text.Parser.Char.char '\n'  )
        <|> void (Text.Parser.Char.text "\r\n")

textLiteral :: Parser a -> Parser (Expr Src a)
textLiteral embedded = (do
    literal <- doubleQuotedLiteral embedded <|> singleQuoteLiteral embedded
    whitespace
    return (TextLit literal) ) <?> "text literal"

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

_constructors :: Parser ()
_constructors = reserved "constructors"

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

localRaw :: Parser ImportType
localRaw =
    choice
        [ parentPath
        , herePath
        , homePath
        , try absolutePath
        ]
  where
    parentPath = do
        _    <- ".." :: Parser Text
        file <- file_

        return (Local Parent file)

    herePath = do
        _    <- "." :: Parser Text
        file <- file_

        return (Local Here file)

    homePath = do
        _    <- "~" :: Parser Text
        file <- file_

        return (Local Home file)

    absolutePath = do
        file <- file_

        return (Local Absolute file)

local :: Parser ImportType
local = do
    a <- localRaw
    whitespace
    return a

scheme :: Parser Text
scheme = "http" <> option "s"

httpRaw :: Parser (Text, File, Text)
httpRaw = do
    prefixText <- scheme <> "://" <> authority
    file   <- file_
    suffixText <- option ("?" <> query) <> option ("#" <> fragment)

    return (prefixText, file, suffixText)

authority :: Parser Text
authority = option (try (userinfo <> "@")) <> host <> option (":" <> port)

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

query :: Parser Text
query = star (pchar <|> satisfy predicate)
  where
    predicate c = c == '/' || c == '?'

fragment :: Parser Text
fragment = star (pchar <|> satisfy predicate)
  where
    predicate c = c == '/' || c == '?'

pctEncoded :: Parser Text
pctEncoded = "%" <> count 2 (satisfy hexdig)

unreserved :: Char -> Bool
unreserved c =
    alpha c || digit c || c == '-' || c == '.' || c == '_' || c == '~'

subDelims :: Char -> Bool
subDelims c = c `elem` ("!$&'()*+,;=" :: String)

http :: Parser ImportType
http = do
    (prefix, path, suffix) <- httpRaw
    whitespace
    headers <- optional (do
        _using
        importHashed_ )
    return (URL prefix path suffix headers)

env :: Parser ImportType
env = do
    _ <- Text.Parser.Char.text "env:"
    a <- (alternative0 <|> alternative1)
    whitespace
    return (Env a)
  where
    alternative0 = do
        bashEnvironmentVariable

    alternative1 = do
        _ <- Text.Parser.Char.char '"'
        a <- posixEnvironmentVariable
        _ <- Text.Parser.Char.char '"'
        return a

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

expression :: Parser a -> Parser (Expr Src a)
expression embedded =
    (   noted
        ( choice
            [ alternative0
            , alternative1
            , alternative2
            , alternative3
            , alternative4
            ]
        )
    <|> alternative5
    ) <?> "expression"
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
    noted
        ( choice
            [ alternative0
            , try alternative1
            , alternative2
            ]
        )
  where
    alternative0 = do
        _merge
        a <- selectorExpression embedded
        b <- selectorExpression embedded
        c <- optional (do
            _colon
            applicationExpression embedded )
        return (Merge a b c)

    alternative1 = (do
        _openBracket
        (emptyCollection embedded <|> nonEmptyOptional embedded) )
        <?> "list literal"

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
    return (a b)
  where
    alternative0 = do
        _List
        return (\a -> ListLit (Just a) empty)

    alternative1 = do
        _Optional
        return (\a -> OptionalLit a empty)

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
makeOperatorExpression subExpression operatorParser operator embedded =
    noted (do
        a <- subExpression embedded
        b <- many (do operatorParser; subExpression embedded)
        return (foldr1 operator (a:b)) )

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
    makeOperatorExpression combineTypesExpression _prefer Prefer

combineTypesExpression :: Parser a -> Parser (Expr Src a)
combineTypesExpression =
    makeOperatorExpression timesExpression _combineTypes CombineTypes

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
    f <- (do _constructors; return Constructors) <|> return id
    a <- noted (selectorExpression embedded)
    b <- many (noted (selectorExpression embedded))
    return (foldl app (f a) b)
  where
    app nL@(Note (Src before _ bytesL) _) nR@(Note (Src _ after bytesR) _) =
        Note (Src before after (bytesL <> bytesR)) (App nL nR)
    app nL nR =
        App nL nR

selectorExpression :: Parser a -> Parser (Expr Src a)
selectorExpression embedded = noted (do
    a <- primitiveExpression embedded

    let left  x  e = Field   e x
    let right xs e = Project e xs
    b <- many (try (do _dot; fmap left label <|> fmap right labels))
    return (foldl (\e k -> k e) a b) )

primitiveExpression :: Parser a -> Parser (Expr Src a)
primitiveExpression embedded =
    noted
        ( choice
            [ alternative00
            , alternative01
            , alternative02
            , alternative03
            , alternative04
            , alternative05
            , alternative06
            , alternative07
            , alternative37

            , choice
                [ alternative08
                , alternative09
                , alternative10
                , alternative11
                , alternative12
                , alternative13
                , alternative14
                , alternative15
                , alternative16
                , alternative17
                , alternative18
                , alternative19
                , alternative20
                , alternative21
                , alternative22
                , alternative23
                , alternative24
                , alternative25
                , alternative26
                , alternative27
                , alternative28
                , alternative29
                , alternative30
                , alternative31
                , alternative32
                , alternative33
                , alternative34
                , alternative35
                , alternative36
                ] <?> "built-in expression"
            ]
        )
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

    alternative04 = (do
        _openBrace
        a <- recordTypeOrLiteral embedded
        _closeBrace
        return a ) <?> "record type or literal"

    alternative05 = (do
        _openAngle
        a <- unionTypeOrLiteral embedded
        _closeAngle
        return a ) <?> "union type or literal"

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
    choice
        [ alternative0
        , alternative1
        , alternative2
        ]
  where
    alternative0 = do
        _equal
        return (RecordLit Data.HashMap.Strict.InsOrd.empty)

    alternative1 = nonEmptyRecordTypeOrLiteral embedded

    alternative2 = return (Record Data.HashMap.Strict.InsOrd.empty)

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
            return (Record (Data.HashMap.Strict.InsOrd.fromList ((a, b):e)))

    let nonEmptyRecordLiteral = do
            _equal
            b <- expression embedded
            e <- many (do
                _comma
                c <- label
                _equal
                d <- expression embedded
                return (c, d) )
            return (RecordLit (Data.HashMap.Strict.InsOrd.fromList ((a, b):e)))

    nonEmptyRecordType <|> nonEmptyRecordLiteral

unionTypeOrLiteral :: Parser a -> Parser (Expr Src a)
unionTypeOrLiteral embedded =
        nonEmptyUnionTypeOrLiteral embedded
    <|> return (Union Data.HashMap.Strict.InsOrd.empty)

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
nonEmptyListLiteral embedded = (do
    _openBracket
    a <- expression embedded
    b <- many (do _comma; expression embedded)
    _closeBracket
    return (ListLit Nothing (Data.Sequence.fromList (a:b))) ) <?> "list literal"

completeExpression :: Parser a -> Parser (Expr Src a)
completeExpression embedded = do
    whitespace
    expression embedded

toMap :: [(Text, a)] -> Parser (InsOrdHashMap Text a)
toMap kvs = do
    let adapt (k, v) = (k, pure v)
    let m = fromListWith (<|>) (fmap adapt kvs)
    let action k vs = case Data.Sequence.viewl vs of
            EmptyL  -> empty
            v :< vs' ->
                if null vs'
                then pure v
                else
                    Text.Parser.Combinators.unexpected
                        ("duplicate field: " ++ Data.Text.unpack k)
    Data.HashMap.Strict.InsOrd.traverseWithKey action m
  where
    fromListWith combine = Data.List.foldl' snoc nil
      where
        nil = Data.HashMap.Strict.InsOrd.empty

        snoc m (k, v) = Data.HashMap.Strict.InsOrd.insertWith combine k v m

-- | Parser for a top-level Dhall expression
expr :: Parser (Expr Src Import)
expr = exprA import_

-- | Parser for a top-level Dhall expression. The expression is parameterized
-- over any parseable type, allowing the language to be extended as needed.
exprA :: Parser a -> Parser (Expr Src a)
exprA = completeExpression

importType_ :: Parser ImportType
importType_ = choice [ local, http, env ]

importHashed_ :: Parser ImportHashed
importHashed_ = do
    importType <- importType_
    hash       <- optional importHash_
    return (ImportHashed {..})
  where
    importHash_ = do
        _ <- Text.Parser.Char.text "sha256:"
        text <- count 64 (satisfy hexdig <?> "hex digit")
        whitespace
        let strictBytes16 = Data.Text.Encoding.encodeUtf8 text
        strictBytes <- case Data.ByteArray.Encoding.convertFromBase Base16 strictBytes16 of
            Left  string      -> fail string
            Right strictBytes -> return (strictBytes :: Data.ByteString.ByteString)
        case Crypto.Hash.digestFromByteString strictBytes of
          Nothing -> fail "Invalid sha256 hash"
          Just h -> pure h

import_ :: Parser Import
import_ = (do
    importHashed <- importHashed_
    importMode   <- alternative <|> pure Code
    return (Import {..}) ) <?> "import"
  where
    alternative = do
        _as
        _Text
        return RawText

-- | A parsing error
data ParseError = ParseError
    { unwrap :: Text.Megaparsec.ParseError Char Void
    , input  :: Text
    }

instance Show ParseError where
    show (ParseError {..}) =
      "\n\ESC[1;31mError\ESC[0m: Invalid input\n\n" <> Text.Megaparsec.parseErrorPretty' input unwrap

instance Exception ParseError

-- | Parse an expression from `Text` containing a Dhall program
exprFromText :: String -> Text -> Either ParseError (Expr Src Import)
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
    :: String
    -> Text
    -> Either ParseError (Text, Expr Src Import)
exprAndHeaderFromText delta text = case result of
    Left errInfo   -> Left (ParseError { unwrap = errInfo, input = text })
    Right (txt, r) -> Right (Data.Text.dropWhileEnd (/= '\n') txt, r)
  where
    parser = do
        (bytes, _) <- Text.Megaparsec.match whitespace
        r <- expr
        Text.Megaparsec.eof
        return (bytes, r)

    result = Text.Megaparsec.parse (unParser parser) delta text
