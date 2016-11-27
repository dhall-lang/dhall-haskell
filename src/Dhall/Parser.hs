{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Dhall.Parser (
    -- * Utilities
      exprFromText

    -- * Parsers
    , exprA

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
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Dhall.Core (Const(..), Expr(..), Path(..), Var(..))
import Filesystem.Path (FilePath)
import Prelude hiding (FilePath, const, pi)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Text.Parser.Combinators (choice, try, (<?>))
import Text.Parser.Expression (Assoc(..), Operator(..))
import Text.Parser.Token (IdentifierStyle(..), TokenParsing(..))
import Text.Parser.Token.Highlight (Highlight(..))
import Text.Parser.Token.Style (CommentStyle(..))
import Text.Trifecta
    (CharParsing, DeltaParsing, MarkParsing, Parsing, Result(..))
import Text.Trifecta.Delta (Delta)

import qualified Data.Char
import qualified Data.HashSet
import qualified Data.Map
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Vector
import qualified Dhall.Core
import qualified Filesystem.Path.CurrentOS
import qualified Text.Parser.Char
import qualified Text.Parser.Combinators
import qualified Text.Parser.Expression
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Style
import qualified Text.PrettyPrint.ANSI.Leijen
import qualified Text.Trifecta
import qualified Text.Trifecta.Combinators
import qualified Text.Trifecta.Delta

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
        , "Natural"
        , "Natural/fold"
        , "Natural/build"
        , "Natural/isZero"
        , "Natural/even"
        , "Natural/odd"
        , "Integer"
        , "Double"
        , "Text"
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
    before        <- Text.Trifecta.position
    (expr, bytes) <- Text.Trifecta.slicedWith (,) parser
    after         <- Text.Trifecta.position
    return (Note (Src before after bytes) expr)

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

lambda :: Parser ()
lambda = symbol "\\" <|> symbol "λ"

pi :: Parser ()
pi = reserve "forall" <|> reserve "∀"

arrow :: Parser ()
arrow = symbol "->" <|> symbol "→"

combine :: Parser ()
combine = symbol "/\\" <|> symbol "∧"

label :: Parser Text
label = Text.Parser.Token.ident identifierStyle <?> "label"

exprA :: Parser (Expr Src Path)
exprA = do
    a <- exprB

    let exprA0 = do
            symbol ":"
            b <- exprA
            return (Annot a b)

    let exprA1 = pure a

    exprA0 <|> exprA1

exprB :: Parser (Expr Src Path)
exprB = choice
    [   noted      exprB0
    ,   noted      exprB1
    ,   noted      exprB3
    ,   noted      exprB5
    ,   noted      exprB6
    ,   noted      exprB7
    ,   noted (try exprB2)
    ,              exprB8
    ]
  where
    exprB0 = do
        lambda
        symbol "("
        a <- label
        symbol ":"
        b <- exprA
        symbol ")"
        arrow
        c <- exprB
        return (Lam a b c)

    exprB1 = do
        reserve "if"
        a <- exprA
        reserve "then"
        b <- exprB
        reserve "else"
        c <- exprC
        return (BoolIf a b c)

    exprB2 = do
        a <- exprC
        arrow
        b <- exprB
        return (Pi "_" a b)

    exprB3 = do
        pi
        symbol "("
        a <- label
        symbol ":"
        b <- exprA
        symbol ")"
        arrow
        c <- exprB
        return (Pi a b c)

    exprB5 = do
        reserve "let"
        a <- label
        b <- optional (do
            symbol ":"
            exprA )
        symbol "="
        c <- exprA
        reserve "in"
        d <- exprB
        return (Let a b c d)

    exprB6 = do
        symbol "["
        a <- elems
        symbol "]"
        symbol ":"
        b <- listLike
        c <- exprE
        return (b c (Data.Vector.fromList a))

    exprB7 = do
        reserve "merge"
        a <- exprE
        b <- exprE
        symbol ":"
        c <- exprD
        return (Merge a b c)

    exprB8 = exprC

listLike :: Parser (Expr Src Path -> Vector (Expr Src Path) -> Expr Src Path)
listLike =
    (   listLike0
    <|> listLike1
    )
  where
    listLike0 = do
        reserve "List"
        return ListLit

    listLike1 = do
        reserve "Optional"
        return OptionalLit

exprC :: Parser (Expr Src Path)
exprC = exprC0
  where
    chain pA pOp op pB = noted (do
        a <- pA
        try (do pOp <?> "operator"; b <- pB; return (op a b)) <|> pure a )

    exprC0 = chain exprC1 (symbol "||") BoolOr       exprC0
    exprC1 = chain exprC2 (symbol "+" ) NaturalPlus  exprC1
    exprC2 = chain exprC3 (symbol "++") TextAppend   exprC2
    exprC3 = chain exprC4 (symbol "&&") BoolAnd      exprC3
    exprC4 = chain exprC5  combine      Combine      exprC4
    exprC5 = chain exprC6 (symbol "*" ) NaturalTimes exprC5
    exprC6 = chain exprC7 (symbol "==") BoolEQ       exprC6
    exprC7 = chain exprD  (symbol "!=") BoolNE       exprC7

-- We can't use left-recursion to define `exprD` otherwise the parser will
-- loop infinitely. However, I'd still like to use left-recursion in the
-- definition because left recursion greatly simplifies the use of `noted`.  The
-- work-around is to parse in two phases:
--
-- * First, parse to count how many arguments the function is applied to
-- * Second, restart the parse using left recursion bounded by the number of
--   arguments
exprD :: Parser (Expr Src Path)
exprD = do
    es <- some (noted exprE)
    let app nL@(Note (Src before _ bytesL) eL) nR@(Note (Src _ after bytesR) eR) =
            Note (Src before after (bytesL <> bytesR)) (App nL nR)
        app _ _ = Dhall.Core.internalError
            ("Dhall.Parser.exprD: foldl1 app (" <> Data.Text.pack (show es) <> ")")
    return (Data.List.foldl1 app es)

exprE :: Parser (Expr Src Path)
exprE = noted (do
    a <- exprF
    b <- many (try (do
        symbol "."
        label ))
    return (Data.List.foldl Field a b) )

exprF :: Parser (Expr Src Path)
exprF = choice
    [   noted (try exprF25)
    ,   noted (try exprF26)
    ,   noted      exprF24
    ,   noted      exprF27
    ,   noted (try exprF28)
    ,   noted      exprF29
    ,   noted (try exprF30)
    ,   noted      exprF31
    ,   noted      exprF32
    ,   (choice
            [   noted      exprF03
            ,   noted      exprF04
            ,   noted      exprF05
            ,   noted      exprF06
            ,   noted      exprF07
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
    ,              exprF33
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
        Text.Parser.Char.char '+'
        a <- Text.Parser.Token.natural
        return (NaturalLit (fromIntegral a)) ) <?> "natural"

    exprF26 = do
        sign <- fmap (\_ -> negate) (Text.Parser.Char.char '-') <|> pure id
        a <- Text.Parser.Token.double
        return (DoubleLit (sign a))

    exprF27 = do
        a <- Text.Parser.Token.stringLiteral
        return (TextLit a)

    exprF28 = record <?> "record type"

    exprF29 = recordLit <?> "record literal"

    exprF30 = union <?> "union type"

    exprF31 = unionLit <?> "union literal"

    exprF32 = do
        a <- import_ <?> "import"
        return (Embed a)

    exprF33 = do
        symbol "("
        a <- exprA
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
            Just b  -> b
            Nothing -> 0
    return (V a b)

elems :: Parser [Expr Src Path]
elems = Text.Parser.Combinators.sepBy exprA (symbol ",")

recordLit :: Parser (Expr Src Path)
recordLit =
        recordLit0
    <|> recordLit1
  where
    recordLit0 = do
        symbol "{=}"
        return (RecordLit (Data.Map.fromList []))

    recordLit1 = do
        symbol "{"
        a <- fieldValues
        b <- toMap a
        symbol "}"
        return (RecordLit b)

fieldValues :: Parser [(Text, Expr Src Path)]
fieldValues =
    Text.Parser.Combinators.sepBy1 fieldValue (symbol ",")

fieldValue :: Parser (Text, Expr Src Path)
fieldValue = do
    a <- label
    symbol "="
    b <- exprA
    return (a, b)

record :: Parser (Expr Src Path)
record = do
    symbol "{"
    a <- fieldTypes
    b <- toMap a
    symbol "}"
    return (Record b)

fieldTypes :: Parser [(Text, Expr Src Path)]
fieldTypes =
    Text.Parser.Combinators.sepBy fieldType (symbol ",")

fieldType :: Parser (Text, Expr Src Path)
fieldType = do
    a <- label
    symbol ":"
    b <- exprA
    return (a, b)

union :: Parser (Expr Src Path)
union = do
    symbol "<"
    a <- alternativeTypes
    b <- toMap a
    symbol ">"
    return (Union b)

alternativeTypes :: Parser [(Text, Expr Src Path)]
alternativeTypes =
    Text.Parser.Combinators.sepBy alternativeType (symbol "|")

alternativeType :: Parser (Text, Expr Src Path)
alternativeType = do
    a <- label
    symbol ":"
    b <- exprA
    return (a, b)

unionLit :: Parser (Expr Src Path)
unionLit =
        try unionLit0
    <|>     unionLit1
  where
    unionLit0 = do
        symbol "<"
        a <- label
        symbol "="
        b <- exprA
        symbol ">"
        return (UnionLit a b Data.Map.empty)

    unionLit1 = do
        symbol "<"
        a <- label
        symbol "="
        b <- exprA
        symbol "|"
        c <- alternativeTypes
        d <- toMap c
        symbol ">"
        return (UnionLit a b d)

import_ :: Parser Path
import_ = do
    a <- import0 <|> import1
    Text.Parser.Token.whiteSpace
    return a
  where
    import0 = do
        a <- file
        return (File a)

    import1 = do
        a <- url
        return (URL a)

file :: Parser FilePath
file =  try (token file0)
    <|>      token file1
    <|>      token file2
  where
    file0 = do
        a <- Text.Parser.Char.string "/"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        case b of
            '\\':_ -> empty -- So that "/\" parses as the operator and not a path
            _      -> return ()
        return (Filesystem.Path.CurrentOS.decodeString (a <> b))

    file1 = do
        a <- Text.Parser.Char.string "./"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        return (Filesystem.Path.CurrentOS.decodeString (a <> b))

    file2 = do
        a <- Text.Parser.Char.string "../"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        return (Filesystem.Path.CurrentOS.decodeString (a <> b))

url :: Parser Text
url =   try url0
    <|> url1
  where
    url0 = do
        a <- Text.Parser.Char.string "https://"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        return (Data.Text.Lazy.pack (a <> b))

    url1 = do
        a <- Text.Parser.Char.string "http://"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        return (Data.Text.Lazy.pack (a <> b))

-- | A parsing error
newtype ParseError = ParseError Doc deriving (Typeable)

instance Show ParseError where
    show (ParseError doc) = show doc

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
        r <- exprA
        Text.Parser.Combinators.eof
        return r )

    result = Text.Trifecta.parseString parser delta string
