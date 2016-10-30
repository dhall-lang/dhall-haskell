{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Dhall.Parser2 where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Vector (Vector)
import Dhall.Core (Const(..), Expr(..), Path(..), Var(..))
import Filesystem.Path (FilePath)
import Prelude hiding (FilePath, const)
import Text.Parser.Combinators (try)
import Text.Parser.Expression (Assoc(..), Operator(..))
import Text.Parser.Token (IdentifierStyle(..), TokenParsing(..))
import Text.Parser.Token.Highlight (Highlight(..))
import Text.Parser.Token.Style (CommentStyle(..))
import Text.Trifecta (CharParsing, DeltaParsing, Parsing)
import Text.Trifecta.Delta (Delta)

import qualified Data.Char
import qualified Data.HashSet
import qualified Data.List
import qualified Data.Map
import qualified Data.Text.Lazy
import qualified Data.Vector
import qualified Filesystem.Path.CurrentOS
import qualified Text.Parser.Char
import qualified Text.Parser.Combinators
import qualified Text.Parser.Expression
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Style
import qualified Text.Trifecta

-- TODO: Go through alternatives and see which ones require `try`

data Src = Src Delta Delta ByteString deriving (Show)

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
    )

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
    -- TODO: Restrict start and letter to ASCII letters
    , _styleStart    = Text.Parser.Char.letter   <|> Text.Parser.Char.char '_'
    , _styleLetter   = Text.Parser.Char.alphaNum <|> Text.Parser.Char.oneOf "_\\"
    , _styleReserved = Data.HashSet.fromList
        -- TODO: Ensure that this list is complete
        [ "let"
        , "in"
        , "Type"
        , "Kind"
        , "forall"
        , "Bool"
        , "True"
        , "False"
        , "apply"
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
        , "Maybe"
        , "Maybe/fold"
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

symbol :: String -> Parser ()
symbol string = do
    _ <- Text.Parser.Token.reserve identifierStyle string
    return ()

lambda :: Parser ()
lambda = symbol "\\" <|> symbol "λ"

arrow :: Parser ()
arrow = symbol "->" <|> symbol "→"

merge :: Parser ()
merge = symbol "/\\" <|> symbol "∧"

label :: Parser Text
label = Text.Parser.Token.ident identifierStyle

exprA :: Parser (Expr Src Path)
exprA = noted
    (   try exprA0
    )
    <|>     exprA1
  where
    exprA0 = do
        a <- exprB
        symbol ":"
        b <- exprA
        return (Annot a b)

    exprA1 = exprB

exprB :: Parser (Expr Src Path)
exprB = noted 
    (   try exprB0
    <|> try exprB1
    <|> try exprB2
    <|> try exprB3
    <|> try exprB4
    <|> try exprB5
    <|> try exprB6
    <|> try exprB7
    )
    <|>     exprB8
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
        symbol "if"
        a <- exprA
        symbol "then"
        b <- exprB
        symbol "else"
        c <- exprC
        return (BoolIf a b c)

    exprB2 = do
        a <- exprC
        arrow
        b <- exprB
        return (Pi "_" a b)

    exprB3 = do
        symbol "forall"
        symbol "("
        a <- label
        symbol ":"
        b <- exprA
        arrow
        c <- exprB
        return (Pi a b c)

    exprB4 = do
        symbol "let"
        a <- label
        symbol "="
        b <- exprA
        symbol "in"
        c <- exprB
        return (Let a Nothing b c)

    exprB5 = do
        symbol "let"
        a <- label
        symbol ":"
        b <- exprA
        symbol "="
        c <- exprA
        symbol "in"
        d <- exprB
        return (Let a (Just b) c d)

    exprB6 = do
        symbol "["
        a <- elems
        symbol "]"
        symbol ":"
        b <- listLike
        c <- exprE
        return (b c (Data.Vector.fromList a))

    exprB7 = do
        symbol "apply"
        a <- exprE
        b <- exprE
        symbol ":"
        c <- exprD
        return (Apply a b c)

    exprB8 = exprC

listLike :: Parser (Expr Src Path -> Vector (Expr Src Path) -> Expr Src Path)
listLike =
    (   listLike0
    <|> listLike1
    )
  where
    listLike0 = do
        symbol "List"
        return ListLit

    listLike1 = do
        symbol "Maybe"
        return MaybeLit

-- TODO: Add `noted` in the right places here
exprC :: Parser (Expr Src Path)
exprC = expressionParser
  where
    expressionParser = Text.Parser.Expression.buildExpressionParser
        [ [ operator BoolAnd      (symbol "&&")
          , operator NaturalTimes (symbol "*" )
          , operator Merge         merge
          ]
        , [ operator BoolOr       (symbol "||")
          , operator NaturalPlus  (symbol "+" )
          , operator TextAppend   (symbol "++")
          ]
        , [ operator BoolEQ       (symbol "==")
          , operator BoolNE       (symbol "/=")
          ]
        ]
        exprD

    operator op parser = Infix (do parser; return op) AssocRight

-- TODO: Add `noted` here
exprD :: Parser (Expr Src Path)
exprD = do
    exprs <- some exprE
    return (Data.List.foldl1 App exprs)

exprE :: Parser (Expr Src Path)
exprE = noted
    (   exprE00
    <|> exprE01
    <|> exprE02
    <|> exprE03
    <|> exprE04
    <|> exprE05
    <|> exprE06
    <|> exprE07
    <|> exprE08
    <|> exprE09
    <|> exprE10
    <|> exprE11
    <|> exprE12
    <|> exprE13
    <|> exprE14
    <|> exprE15
    <|> exprE16
    <|> exprE17
    <|> exprE18
    <|> exprE19
    <|> exprE20
    <|> exprE21
    <|> exprE22
    <|> exprE23
    <|> exprE24
    <|> exprE25
    <|> exprE26
    <|> exprE27
    <|> exprE28
    <|> exprE29
    <|> exprE30
    <|> exprE31
--  <|> exprE32
    <|> exprE33
    )
  where
    exprE00 = do
        a <- var
        return (Var a)

    exprE01 = do
        a <- const
        return (Const a)

    exprE02 = do
        symbol "Natural"
        return Natural

    exprE03 = do
        symbol "Natural/fold"
        return NaturalFold

    exprE04 = do
        symbol "Natural/build"
        return NaturalBuild

    exprE05 = do
        symbol "Natural/isZero"
        return NaturalIsZero

    exprE06 = do
        symbol "Natural/even"
        return NaturalEven

    exprE07 = do
        symbol "Natural/odd"
        return NaturalOdd

    exprE08 = do
        symbol "Integer"
        return Integer

    exprE09 = do
        symbol "Double"
        return Double

    exprE10 = do
        symbol "Text"
        return Text

    exprE11 = do
        symbol "List"
        return List

    exprE12 = do
        symbol "List/build"
        return ListBuild

    exprE13 = do
        symbol "List/fold"
        return ListFold

    exprE14 = do
        symbol "List/length"
        return ListLength

    exprE15 = do
        symbol "List/head"
        return ListHead

    exprE16 = do
        symbol "List/last"
        return ListLast

    exprE17 = do
        symbol "List/indexed"
        return ListIndexed

    exprE18 = do
        symbol "List/reverse"
        return ListReverse

    exprE19 = do
        symbol "Maybe"
        return Maybe

    exprE20 = do
        symbol "Maybe/fold"
        return MaybeFold

    exprE21 = do
        symbol "True"
        return (BoolLit True)

    exprE22 = do
        symbol "False"
        return (BoolLit False)

    exprE23 = do
        a <- Text.Parser.Token.natural
        return (IntegerLit a)

    exprE24 = do
        Text.Parser.Char.char '+'
        a <- Text.Parser.Token.natural
        return (NaturalLit (fromIntegral a))

    exprE25 = do
        a <- Text.Parser.Token.double
        return (DoubleLit a)

    exprE26 = do
        a <- Text.Parser.Token.stringLiteral
        return (TextLit a)

    exprE27 = record

    exprE28 = recordLit

    exprE29 = union

    exprE30 = unionLit

    exprE31 = do
        a <- import_
        return (Embed a)

--  exprE32 = do
--      a <- exprE
--      symbol "."
--      b <- label
--      return (Field a b)

    exprE33 = do
        symbol "("
        a <- exprA
        symbol ")"
        return a

const :: Parser Const
const = const0
    <|> const1
  where
    const0 = do
        symbol "Type"
        return Type

    const1 = do
        symbol "Kind"
        return Kind

var :: Parser Var
var =   var0
    <|> var1
  where
    var0 = do
        a <- label
        return (V a 0)

    var1 = do
        a <- label
        symbol "@"
        b <- Text.Parser.Token.natural
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
        symbol "}"
        return (RecordLit (Data.Map.fromList a))

fieldValues :: Parser [(Text, Expr Src Path)]
fieldValues = Text.Parser.Combinators.sepBy1 fieldValue (symbol ",")

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
    symbol "}"
    return (Record (Data.Map.fromList a))

fieldTypes :: Parser [(Text, Expr Src Path)]
fieldTypes = Text.Parser.Combinators.sepBy fieldType (symbol ",")

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
    symbol ">"
    return (Union (Data.Map.fromList a))

alternativeTypes :: Parser [(Text, Expr Src Path)]
alternativeTypes = Text.Parser.Combinators.sepBy alternativeType (symbol "|")

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
        symbol ">"
        return (UnionLit a b (Data.Map.fromList c))

import_ :: Parser Path
import_ =
        import0
    <|> import1
  where
    import0 = do
        a <- file
        return (File a)

    import1 = do
        a <- url
        return (URL a)

file :: Parser FilePath
file =  token file0
    <|> token file1
    <|> token file2
  where
    file0 = do
        a <- Text.Parser.Char.string "/"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
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
url =   url0
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
