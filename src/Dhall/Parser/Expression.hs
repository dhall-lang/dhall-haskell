{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Parsing Dhall expressions.
module Dhall.Parser.Expression where

import Control.Applicative (Alternative(..), liftA2, optional)
import Control.Exception (Exception)
import Control.Monad (MonadPlus)
import Data.ByteArray.Encoding (Base(..))
import Data.Data (Data)
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

import Dhall.Parser.Combinators
import Dhall.Parser.Token
import Dhall.Parser.Literal

noted :: Parser (Expr Src a) -> Parser (Expr Src a)
noted parser = do
    before      <- Text.Megaparsec.getPosition
    (tokens, e) <- Text.Megaparsec.match parser
    after       <- Text.Megaparsec.getPosition
    let src₀ = Src before after tokens
    case e of
        Note src₁ _ | src₀ == src₁ -> return e
        _                          -> return (Note src₀ e)

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
    a <- noted (importExpression embedded)
    b <- many (noted (importExpression embedded))
    return (foldl app (f a) b)
  where
    app nL@(Note (Src before _ bytesL) _) nR@(Note (Src _ after bytesR) _) =
        Note (Src before after (bytesL <> bytesR)) (App nL nR)
    app nL nR =
        App nL nR

importExpression :: Parser a -> Parser (Expr Src a)
importExpression embedded = noted (choice [ alternative0, alternative1 ])
  where
    alternative0 = do
        a <- embedded
        return (Embed a)

    alternative1 = selectorExpression embedded

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
            m <- toMap ((a, b) : e)
            return (Record m)

    let nonEmptyRecordLiteral = do
            _equal
            b <- expression embedded
            e <- many (do
                _comma
                c <- label
                _equal
                d <- expression embedded
                return (c, d) )
            m <- toMap ((a, b) : e)
            return (RecordLit m)

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

env :: Parser ImportType
env = do
    _ <- Text.Parser.Char.text "env:"
    a <- (alternative0 <|> alternative1)
    whitespace
    return (Env a)
  where
    alternative0 = bashEnvironmentVariable

    alternative1 = do
        _ <- Text.Parser.Char.char '"'
        a <- posixEnvironmentVariable
        _ <- Text.Parser.Char.char '"'
        return a

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
        File (Directory segments) final <- file_

        return (Local Here (File (Directory (segments ++ [".."])) final))

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

http :: Parser ImportType
http = do
    (prefix, path, suffix) <- httpRaw
    whitespace
    headers <- optional (do
        _using
        importHashed_ )
    return (URL prefix path suffix headers)

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
          Just h  -> pure h

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
