{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing Dhall expressions.
module Dhall.Parser.Expression where

import Control.Applicative (Alternative(..), optional)
import Data.ByteArray.Encoding (Base(..))
import Data.Functor (void)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Dhall.Core
import Prelude hiding (const, pi)
import Text.Parser.Combinators (choice, try, (<?>))

import qualified Crypto.Hash
import qualified Data.ByteArray.Encoding
import qualified Data.ByteString
import qualified Data.Char
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Text.Megaparsec
import qualified Text.Parser.Char

import Dhall.Parser.Combinators
import Dhall.Parser.Token

noted :: Parser (Expr Src a) -> Parser (Expr Src a)
noted parser = do
    before      <- Text.Megaparsec.getSourcePos
    (tokens, e) <- Text.Megaparsec.match parser
    after       <- Text.Megaparsec.getSourcePos
    let src₀ = Src before after tokens
    case e of
        Note src₁ _ | laxSrcEq src₀ src₁ -> return e
        _                                -> return (Note src₀ e)

completeExpression :: Parser a -> Parser (Expr Src a)
completeExpression embedded = completeExpression_
  where
    completeExpression_ = do
        whitespace
        expression

    expression =
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
            b <- expression
            _closeParens
            _arrow
            c <- expression
            return (Lam a b c)

        alternative1 = do
            _if
            a <- expression
            _then
            b <- expression
            _else
            c <- expression
            return (BoolIf a b c)

        alternative2 = do
            _let
            a <- label
            b <- optional (do
                _colon
                expression )
            _equal
            c <- expression
            _in
            d <- expression
            return (Let a b c d)

        alternative3 = do
            _forall
            _openParens
            a <- label
            _colon
            b <- expression
            _closeParens
            _arrow
            c <- expression
            return (Pi a b c)

        alternative4 = do
            a <- operatorExpression

            let alternative4A = do
                    _arrow
                    b <- expression
                    return (Pi "_" a b)

            let alternative4B = do
                    _colon

                    b <- expression

                    case (denote a, denote b) of
                        (ListLit _ xs, App List c) ->
                            return (ListLit (Just c) xs)
                        (ListLit _ [x], App Optional c) ->
                            return (OptionalLit c (Just x))
                        (ListLit _ [], App Optional c) ->
                            return (OptionalLit c Nothing)
                        (Merge c d _, e) ->
                            return (Merge c d (Just e))
                        _ -> return (Annot a b)

            alternative4A <|> alternative4B <|> pure a

        alternative5 = operatorExpression

    operatorExpression = precedence0Expression

    makeOperatorExpression subExpression operatorParser =
            noted (do
                a <- subExpression
                b <- Text.Megaparsec.many $ do
                    op <- operatorParser
                    r  <- subExpression

                    return (\l -> l `op` r)
                return (foldl (\x f -> f x) a b) )

    precedence0Operator =
                ImportAlt   <$ _importAlt
            <|> BoolOr      <$ _or
            <|> TextAppend  <$ _textAppend
            <|> NaturalPlus <$ _plus
            <|> ListAppend  <$ _listAppend

    precedence1Operator =
                BoolAnd     <$ _and
            <|> Combine     <$ _combine

    precedence2Operator =
                Prefer       <$ _prefer
            <|> CombineTypes <$ _combineTypes
            <|> NaturalTimes <$ _times
            <|> BoolNE       <$ _notEqual

    precedence3Operator = BoolEQ <$ _doubleEqual

    precedence0Expression =
            makeOperatorExpression precedence1Expression precedence0Operator

    precedence1Expression =
            makeOperatorExpression precedence2Expression precedence1Operator

    precedence2Expression =
            makeOperatorExpression precedence3Expression precedence2Operator

    precedence3Expression =
            makeOperatorExpression applicationExpression precedence3Operator

    applicationExpression = do
            f <-    (do _constructors; return Constructors)
                <|> (do _Some; return Some)
                <|> return id
            a <- noted importExpression
            b <- Text.Megaparsec.many (noted importExpression)
            return (foldl app (f a) b)
          where
            app nL@(Note (Src before _ bytesL) _) nR@(Note (Src _ after bytesR) _) =
                Note (Src before after (bytesL <> bytesR)) (App nL nR)
            app nL nR =
                App nL nR

    importExpression = noted (choice [ alternative0, alternative1 ])
          where
            alternative0 = do
                a <- embedded
                return (Embed a)

            alternative1 = selectorExpression

    selectorExpression = noted (do
            a <- primitiveExpression

            let left  x  e = Field   e x
            let right xs e = Project e xs
            b <- Text.Megaparsec.many (try (do _dot; fmap left label <|> fmap right labels))
            return (foldl (\e k -> k e) a b) )

    primitiveExpression =
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

                    , builtin <?> "built-in expression"
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

            alternative03 = textLiteral

            alternative04 = (do
                _openBrace
                a <- recordTypeOrLiteral
                _closeBrace
                return a ) <?> "record type or literal"

            alternative05 = (do
                _openAngle
                a <- unionTypeOrLiteral
                _closeAngle
                return a ) <?> "union type or literal"

            alternative06 = listLiteral

            alternative07 = do
                _merge
                a <- importExpression
                b <- importExpression
                return (Merge a b Nothing)

            builtin = do
                let predicate c =
                            c == 'N'
                        ||  c == 'I'
                        ||  c == 'D'
                        ||  c == 'L'
                        ||  c == 'O'
                        ||  c == 'B'
                        ||  c == 'T'
                        ||  c == 'F'
                        ||  c == 'K'

                c <- Text.Megaparsec.lookAhead (Text.Megaparsec.satisfy predicate)

                case c of
                    'N' ->
                        choice
                            [ NaturalFold      <$ _NaturalFold
                            , NaturalBuild     <$ _NaturalBuild
                            , NaturalIsZero    <$ _NaturalIsZero
                            , NaturalEven      <$ _NaturalEven
                            , NaturalOdd       <$ _NaturalOdd
                            , NaturalToInteger <$ _NaturalToInteger
                            , NaturalToInteger <$ _NaturalToInteger
                            , NaturalShow      <$ _NaturalShow
                            , Natural          <$ _Natural
                            , None             <$ _None
                            ]
                    'I' ->
                        choice
                            [ IntegerShow      <$ _IntegerShow
                            , IntegerToDouble  <$ _IntegerToDouble
                            , Integer          <$ _Integer
                            ]

                    'D' ->
                        choice
                            [ DoubleShow       <$ _DoubleShow
                            , Double           <$ _Double
                            ]
                    'L' ->
                        choice
                            [ ListBuild        <$ _ListBuild
                            , ListFold         <$ _ListFold
                            , ListLength       <$ _ListLength
                            , ListHead         <$ _ListHead
                            , ListLast         <$ _ListLast
                            , ListIndexed      <$ _ListIndexed
                            , ListReverse      <$ _ListReverse
                            , List             <$ _List
                            ]
                    'O' ->
                        choice
                            [ OptionalFold     <$ _OptionalFold
                            , OptionalBuild    <$ _OptionalBuild
                            , Optional         <$ _Optional
                            ]
                    'B' ->    Bool             <$ _Bool
                    'T' ->
                        choice
                            [ Text             <$ _Text
                            , BoolLit True     <$ _True
                            , Const Type       <$ _Type
                            ]
                    'F' ->    BoolLit False    <$ _False
                    'K' ->    Const Kind       <$ _Kind
                    _   ->    empty

            alternative37 = do
                a <- identifier
                return (Var a)

            alternative38 = do
                _openParens
                a <- expression
                _closeParens
                return a

    doubleQuotedChunk =
            choice
                [ interpolation
                , unescapedCharacterFast
                , unescapedCharacterSlow
                , escapedCharacter
                ]
          where
            interpolation = do
                _ <- Text.Parser.Char.text "${"
                e <- completeExpression_
                _ <- Text.Parser.Char.char '}'
                return (Chunks [(mempty, e)] mempty)

            unescapedCharacterFast = do
                t <- Text.Megaparsec.takeWhile1P Nothing predicate
                return (Chunks [] t)
              where
                predicate c =
                    (   ('\x20' <= c && c <= '\x21'    )
                    ||  ('\x23' <= c && c <= '\x5B'    )
                    ||  ('\x5D' <= c && c <= '\x10FFFF')
                    ) && c /= '$'

            unescapedCharacterSlow = do
                _ <- Text.Megaparsec.single '$'
                return (Chunks [] "$")

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

    doubleQuotedLiteral = do
            _      <- Text.Parser.Char.char '"'
            chunks <- Text.Megaparsec.many doubleQuotedChunk
            _      <- Text.Parser.Char.char '"'
            return (mconcat chunks)

    singleQuoteContinue =
            choice
                [ escapeSingleQuotes
                , interpolation
                , escapeInterpolation
                , endLiteral
                , unescapedCharacterFast
                , unescapedCharacterSlow
                , tab
                , endOfLine
                ]
          where
                escapeSingleQuotes = do
                    _ <- "'''" :: Parser Text
                    b <- singleQuoteContinue
                    return ("''" <> b)

                interpolation = do
                    _ <- Text.Parser.Char.text "${"
                    a <- completeExpression_
                    _ <- Text.Parser.Char.char '}'
                    b <- singleQuoteContinue
                    return (Chunks [(mempty, a)] mempty <> b)

                escapeInterpolation = do
                    _ <- Text.Parser.Char.text "''${"
                    b <- singleQuoteContinue
                    return ("${" <> b)

                endLiteral = do
                    _ <- Text.Parser.Char.text "''"
                    return mempty

                unescapedCharacterFast = do
                    a <- Text.Megaparsec.takeWhile1P Nothing predicate
                    b <- singleQuoteContinue
                    return (Chunks [] a <> b)
                  where
                    predicate c =
                        ('\x20' <= c && c <= '\x10FFFF') && c /= '$' && c /= '\''

                unescapedCharacterSlow = do
                    a <- satisfy predicate
                    b <- singleQuoteContinue
                    return (Chunks [] a <> b)
                  where
                    predicate c = c == '$' || c == '\''

                endOfLine = do
                    a <- "\n" <|> "\r\n"
                    b <- singleQuoteContinue
                    return (Chunks [] a <> b)

                tab = do
                    _ <- Text.Parser.Char.char '\t'
                    b <- singleQuoteContinue
                    return ("\t" <> b)

    singleQuoteLiteral = do
            _ <- Text.Parser.Char.text "''"

            -- This is technically not in the grammar, but it's still equivalent to the
            -- original grammar and an easy way to discard the first character if it's
            -- a newline
            _ <- optional endOfLine

            a <- singleQuoteContinue

            return (dedent a)
          where
            endOfLine =
                    void (Text.Parser.Char.char '\n'  )
                <|> void (Text.Parser.Char.text "\r\n")

    textLiteral = (do
            literal <- doubleQuotedLiteral <|> singleQuoteLiteral
            whitespace
            return (TextLit literal) ) <?> "text literal"

    recordTypeOrLiteral =
            choice
                [ alternative0
                , alternative1
                , alternative2
                ]
          where
            alternative0 = do
                _equal
                return (RecordLit Data.HashMap.Strict.InsOrd.empty)

            alternative1 = nonEmptyRecordTypeOrLiteral

            alternative2 = return (Record Data.HashMap.Strict.InsOrd.empty)

    nonEmptyRecordTypeOrLiteral = do
            a <- label

            let nonEmptyRecordType = do
                    _colon
                    b <- expression
                    e <- Text.Megaparsec.many (do
                        _comma
                        c <- label
                        _colon
                        d <- expression
                        return (c, d) )
                    m <- toMap ((a, b) : e)
                    return (Record m)

            let nonEmptyRecordLiteral = do
                    _equal
                    b <- expression
                    e <- Text.Megaparsec.many (do
                        _comma
                        c <- label
                        _equal
                        d <- expression
                        return (c, d) )
                    m <- toMap ((a, b) : e)
                    return (RecordLit m)

            nonEmptyRecordType <|> nonEmptyRecordLiteral

    unionTypeOrLiteral =
                nonEmptyUnionTypeOrLiteral
            <|> return (Union Data.HashMap.Strict.InsOrd.empty)

    nonEmptyUnionTypeOrLiteral = do
            (f, kvs) <- loop
            m <- toMap kvs
            return (f m)
          where
            loop = do
                a <- label

                let alternative0 = do
                        _equal
                        b <- expression
                        kvs <- Text.Megaparsec.many (do
                            _bar
                            c <- label
                            _colon
                            d <- expression
                            return (c, d) )
                        return (UnionLit a b, kvs)

                let alternative1 = do
                        _colon
                        b <- expression

                        let alternative2 = do
                                _bar
                                (f, kvs) <- loop
                                return (f, (a, b):kvs)

                        let alternative3 = return (Union, [(a, b)])

                        alternative2 <|> alternative3

                alternative0 <|> alternative1

    listLiteral = (do
            _openBracket
            a <- Text.Megaparsec.sepBy expression _comma
            _closeBracket
            return (ListLit Nothing (Data.Sequence.fromList a)) ) <?> "list literal"

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
    url <- httpRaw
    whitespace
    headers <- optional (do
        _using
        (importHashed_ <|> (_openParens *> importHashed_ <* _closeParens)) )
    return (Remote (url { headers }))

missing :: Parser ImportType
missing = do
  _missing
  return Missing

importType_ :: Parser ImportType
importType_ = do
    let predicate c =
            c == '~' || c == '.' || c == '/' || c == 'h' || c == 'e' || c == 'm'

    _ <- Text.Megaparsec.lookAhead (Text.Megaparsec.satisfy predicate)

    choice [ local, http, env, missing ]

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

-- | Similar to `Dhall.Core.renderChunks` except that this doesn't bother to
-- render interpolated expressions to avoid a `Buildable a` constraint.  The
-- interpolated contents are not necessary for computing how much to dedent a
-- multi-line string
--
-- This also doesn't include the surrounding quotes since they would interfere
-- with the whitespace detection
renderChunks :: Chunks s a -> Text
renderChunks (Chunks a b) = foldMap renderChunk a <> b
  where
    renderChunk :: (Text, Expr s a) -> Text
    renderChunk (c, _) = c <> "${x}"

dedent :: Chunks Src a -> Chunks Src a
dedent chunks0 = process chunks0
  where
    text0 = renderChunks chunks0

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
