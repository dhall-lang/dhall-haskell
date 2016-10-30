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

expr0 :: Parser (Expr Src Path)
expr0 = noted
    (   try expr0A
    )
    <|>     expr0B
  where
    expr0A = do
        a <- expr1
        symbol ":"
        b <- expr0
        return (Annot a b)

    expr0B = expr1

expr1 :: Parser (Expr Src Path)
expr1 = noted 
    (   try expr1A
    <|> try expr1B
    <|> try expr1C
    <|> try expr1D
    <|> try expr1E
    <|> try expr1F
    <|> try expr1G
    <|> try expr1H
    )
    <|>     expr1I
  where
    expr1A = do
        lambda
        symbol "("
        a <- label
        symbol ":"
        b <- expr0
        symbol ")"
        arrow
        c <- expr1
        return (Lam a b c)

    expr1B = do
        symbol "if"
        a <- expr0
        symbol "then"
        b <- expr1
        symbol "else"
        c <- expr2
        return (BoolIf a b c)

    expr1C = do
        a <- expr2
        arrow
        b <- expr1
        return (Pi "_" a b)

    expr1D = do
        symbol "forall"
        symbol "("
        a <- label
        symbol ":"
        b <- expr0
        arrow
        c <- expr1
        return (Pi a b c)

    expr1E = do
        symbol "let"
        a <- label
        symbol "="
        b <- expr0
        symbol "in"
        c <- expr1
        return (Let a Nothing b c)

    expr1F = do
        symbol "let"
        a <- label
        symbol ":"
        b <- expr0
        symbol "="
        c <- expr0
        symbol "in"
        d <- expr1
        return (Let a (Just b) c d)

    expr1G = do
        symbol "["
        a <- elems
        symbol "]"
        symbol ":"
        b <- listLike
        c <- expr6
        return (b c (Data.Vector.fromList a))

    expr1H = do
        symbol "apply"
        a <- expr6
        b <- expr6
        symbol ":"
        c <- expr5
        return (Apply a b c)

    expr1I = expr2

listLike :: Parser (Expr Src Path -> Vector (Expr Src Path) -> Expr Src Path)
listLike =
    (   listLikeA
    <|> listLikeB
    )
  where
    listLikeA = do
        symbol "List"
        return ListLit

    listLikeB = do
        symbol "Maybe"
        return MaybeLit

-- TODO: Add `noted` in the right places here
-- TODO: Change `expr5` to `expr3` and so forth
expr2 :: Parser (Expr Src Path)
expr2 = expressionParser
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
        expr5

    operator op parser = Infix (do parser; return op) AssocRight

-- TODO: Add `noted` here
expr5 :: Parser (Expr Src Path)
expr5 = do
    exprs <- some expr6
    return (Data.List.foldl1 App exprs)

expr6 :: Parser (Expr Src Path)
expr6 = noted
    (   expr6AA
    <|> expr6AB
    <|> expr6AC
    <|> expr6AD
    <|> expr6AE
    <|> expr6AF
    <|> expr6AG
    <|> expr6AH
    <|> expr6AI
    <|> expr6AJ
    <|> expr6AK
    <|> expr6AL
    <|> expr6AM
    <|> expr6AN
    <|> expr6AO
    <|> expr6AP
    <|> expr6AQ
    <|> expr6AR
    <|> expr6AS
    <|> expr6AT
    <|> expr6AU
    <|> expr6AV
    <|> expr6AW
    <|> expr6AX
    <|> expr6AY
    <|> expr6AZ
    <|> expr6BA
    <|> expr6BB
    <|> expr6BC
    <|> expr6BD
    <|> expr6BE
    <|> expr6BF
--  <|> expr6BG
    <|> expr6BH
    )
  where
    expr6AA = do
        a <- var
        return (Var a)

    expr6AB = do
        a <- const
        return (Const a)

    expr6AC = do
        symbol "Natural"
        return Natural

    expr6AD = do
        symbol "Natural/fold"
        return NaturalFold

    expr6AE = do
        symbol "Natural/build"
        return NaturalBuild

    expr6AF = do
        symbol "Natural/isZero"
        return NaturalIsZero

    expr6AG = do
        symbol "Natural/even"
        return NaturalEven

    expr6AH = do
        symbol "Natural/odd"
        return NaturalOdd

    expr6AI = do
        symbol "Integer"
        return Integer

    expr6AJ = do
        symbol "Double"
        return Double

    expr6AK = do
        symbol "Text"
        return Text

    expr6AL = do
        symbol "List"
        return List

    expr6AM = do
        symbol "List/build"
        return ListBuild

    expr6AN = do
        symbol "List/fold"
        return ListFold

    expr6AO = do
        symbol "List/length"
        return ListLength

    expr6AP = do
        symbol "List/head"
        return ListHead

    expr6AQ = do
        symbol "List/last"
        return ListLast

    expr6AR = do
        symbol "List/indexed"
        return ListIndexed

    expr6AS = do
        symbol "List/reverse"
        return ListReverse

    expr6AT = do
        symbol "Maybe"
        return Maybe

    expr6AU = do
        symbol "Maybe/fold"
        return MaybeFold

    expr6AV = do
        symbol "True"
        return (BoolLit True)

    expr6AW = do
        symbol "False"
        return (BoolLit False)

    expr6AX = do
        a <- Text.Parser.Token.natural
        return (IntegerLit a)

    expr6AY = do
        Text.Parser.Char.char '+'
        a <- Text.Parser.Token.natural
        return (NaturalLit (fromIntegral a))

    expr6AZ = do
        a <- Text.Parser.Token.double
        return (DoubleLit a)

    expr6BA = do
        a <- Text.Parser.Token.stringLiteral
        return (TextLit a)

    expr6BB = record

    expr6BC = recordLit

    expr6BD = union

    expr6BE = unionLit

    expr6BF = do
        a <- import_
        return (Embed a)

--  expr6BG = do
--      a <- expr6
--      symbol "."
--      b <- label
--      return (Field a b)

    expr6BH = do
        symbol "("
        a <- expr0
        symbol ")"
        return a

const :: Parser Const
const = constA
    <|> constB
  where
    constA = do
        symbol "Type"
        return Type

    constB = do
        symbol "Kind"
        return Kind

var :: Parser Var
var =   varA
    <|> varB
  where
    varA = do
        a <- label
        return (V a 0)

    varB = do
        a <- label
        symbol "@"
        b <- Text.Parser.Token.natural
        return (V a b)

elems :: Parser [Expr Src Path]
elems = Text.Parser.Combinators.sepBy expr0 (symbol ",")

recordLit :: Parser (Expr Src Path)
recordLit =
        recordLitA
    <|> recordLitB
  where
    recordLitA = do
        symbol "{=}"
        return (RecordLit (Data.Map.fromList []))

    recordLitB = do
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
    b <- expr0
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
    b <- expr0
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
    b <- expr0
    return (a, b)

unionLit :: Parser (Expr Src Path)
unionLit =
        try unionLitA
    <|>     unionLitB
  where
    unionLitA = do
        symbol "<"
        a <- label
        symbol "="
        b <- expr0
        symbol ">"
        return (UnionLit a b Data.Map.empty)

    unionLitB = do
        symbol "<"
        a <- label
        symbol "="
        b <- expr0
        symbol "|"
        c <- alternativeTypes
        symbol ">"
        return (UnionLit a b (Data.Map.fromList c))

import_ :: Parser Path
import_ =
        importA
    <|> importB
  where
    importA = do
        a <- file
        return (File a)

    importB = do
        a <- url
        return (URL a)

file :: Parser FilePath
file =  token fileA
    <|> token fileB
    <|> token fileC
  where
    fileA = do
        a <- Text.Parser.Char.string "/"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        return (Filesystem.Path.CurrentOS.decodeString (a <> b))

    fileB = do
        a <- Text.Parser.Char.string "./"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        return (Filesystem.Path.CurrentOS.decodeString (a <> b))

    fileC = do
        a <- Text.Parser.Char.string "../"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        return (Filesystem.Path.CurrentOS.decodeString (a <> b))

url :: Parser Text
url =   urlA
    <|> urlB
  where
    urlA = do
        a <- Text.Parser.Char.string "https://"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        return (Data.Text.Lazy.pack (a <> b))

    urlB = do
        a <- Text.Parser.Char.string "http://"
        b <- many (Text.Parser.Char.satisfy (not . Data.Char.isSpace))
        return (Data.Text.Lazy.pack (a <> b))
