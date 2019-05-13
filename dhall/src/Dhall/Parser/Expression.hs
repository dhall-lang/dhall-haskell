{-# LANGUAGE CPP                 #-}
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
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Dhall.Core
import Prelude hiding (const, pi)
import Text.Parser.Combinators (choice, try, (<?>))

import qualified Crypto.Hash
import qualified Data.ByteArray.Encoding
import qualified Data.ByteString
import qualified Data.Char
import qualified Data.Foldable
import qualified Data.List.NonEmpty
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Text.Megaparsec
#if !MIN_VERSION_megaparsec(7, 0, 0)
import qualified Text.Megaparsec.Char as Text.Megaparsec
#endif
import qualified Text.Parser.Char

import Dhall.Parser.Combinators
import Dhall.Parser.Token

getSourcePos :: Text.Megaparsec.MonadParsec e s m =>
                m Text.Megaparsec.SourcePos
getSourcePos =
#if MIN_VERSION_megaparsec(7, 0, 0)
    Text.Megaparsec.getSourcePos
#else
    Text.Megaparsec.getPosition
#endif
{-# INLINE getSourcePos #-}

getOffset :: Text.Megaparsec.MonadParsec e s m => m Int
#if MIN_VERSION_megaparsec(7, 0, 0)
getOffset = Text.Megaparsec.stateOffset <$> Text.Megaparsec.getParserState
#else
getOffset = Text.Megaparsec.stateTokensProcessed <$> Text.Megaparsec.getParserState
#endif
{-# INLINE getOffset #-}

setOffset :: Text.Megaparsec.MonadParsec e s m => Int -> m ()
#if MIN_VERSION_megaparsec(7, 0, 0)
setOffset o = Text.Megaparsec.updateParserState $ \(Text.Megaparsec.State s _ pst) ->
  Text.Megaparsec.State s o pst
#else
setOffset o = Text.Megaparsec.updateParserState $ \(Text.Megaparsec.State s p _ stw) ->
  Text.Megaparsec.State s p o stw
#endif
{-# INLINE setOffset #-}

noted :: Parser (Expr Src a) -> Parser (Expr Src a)
noted parser = do
    before      <- getSourcePos
    (tokens, e) <- Text.Megaparsec.match parser
    after       <- getSourcePos
    let src₀ = Src before after tokens
    case e of
        Note src₁ _ | laxSrcEq src₀ src₁ -> return e
        _                                -> return (Note src₀ e)

shallowDenote :: Expr s a -> Expr s a
shallowDenote (Note _ e) = shallowDenote e
shallowDenote         e  = e

completeExpression :: Parser a -> Parser (Expr Src a)
completeExpression embedded = completeExpression_
  where
    completeExpression_ = do
        whitespace
        expression

    expression =
        noted
            ( choice
                [ alternative0
                , alternative1
                , alternative2
                , alternative3
                , alternative4
                ]
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
            let binding = do
                    _let

                    c <- label
                    d <- optional (do
                        _colon
                        expression )

                    _equal

                    e <- expression

                    return (Binding c d e)

            as <- Data.List.NonEmpty.some1 binding

            _in

            b <- expression

            return (Let as b)

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

                    case (shallowDenote a, shallowDenote b) of
                        (ListLit _ xs, App f c) ->
                            case shallowDenote f of
                                List     -> case xs of
                                    [] -> return (ListLit (Just c) xs)
                                    _  -> return (Annot a b)
                                Optional -> case xs of
                                    [x] -> return (OptionalLit c (Just x))
                                    []  -> return (OptionalLit c Nothing)
                                    _   -> return (Annot a b)
                                _ ->
                                    return (Annot a b)
                        (Merge c d _, e) ->
                            return (Merge c d (Just e))
                        _ -> return (Annot a b)

            alternative4A <|> alternative4B <|> pure a

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
                CombineTypes <$ _combineTypes
            <|> Prefer       <$ _prefer
            <|> NaturalTimes <$ _times
            <|> BoolEQ       <$ _doubleEqual

    precedence3Operator = BoolNE <$ _notEqual

    precedence0Expression =
            makeOperatorExpression precedence1Expression precedence0Operator

    precedence1Expression =
            makeOperatorExpression precedence2Expression precedence1Operator

    precedence2Expression =
            makeOperatorExpression precedence3Expression precedence2Operator

    precedence3Expression =
            makeOperatorExpression applicationExpression precedence3Operator

    applicationExpression = do
            f <-    (do _Some; return Some)
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
            b <- Text.Megaparsec.many (try (do _dot; fmap left anyLabel <|> fmap right labels))
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
                    , alternative09

                    , builtin <?> "built-in expression"
                    ]
                )
            <|> alternative38
          where
            alternative00 = do
                n <- getOffset
                a <- try doubleLiteral
                b <- if isInfinite a
                       then setOffset n *> fail "double out of bounds"
                       else return a
                return (DoubleLit b)

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

            alternative09 = do
                a <- try doubleInfinity
                return (DoubleLit a)

            builtin = do
                let predicate c =
                            c == 'N'
                        ||  c == 'I'
                        ||  c == 'D'
                        ||  c == 'L'
                        ||  c == 'O'
                        ||  c == 'B'
                        ||  c == 'S'
                        ||  c == 'T'
                        ||  c == 'F'
                        ||  c == 'K'

                let nan = (0.0/0.0)

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
                            , DoubleLit nan    <$ _NaN
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
                    'S' ->    Const Sort       <$ _Sort
                    'T' ->
                        choice
                            [ TextShow         <$ _TextShow
                            , Text             <$ _Text
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
                _ <- Text.Parser.Char.char '$'
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
            _ <- endOfLine
            a <- singleQuoteContinue

            return (toDoubleQuoted a)
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
                return (RecordLit mempty)

            alternative1 = nonEmptyRecordTypeOrLiteral

            alternative2 = return (Record mempty)

    nonEmptyRecordTypeOrLiteral = do
            a <- anyLabel

            let nonEmptyRecordType = do
                    _colon
                    b <- expression
                    e <- Text.Megaparsec.many (do
                        _comma
                        c <- anyLabel
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
                        c <- anyLabel
                        _equal
                        d <- expression
                        return (c, d) )
                    m <- toMap ((a, b) : e)
                    return (RecordLit m)

            nonEmptyRecordType <|> nonEmptyRecordLiteral

    unionTypeOrLiteral =
                nonEmptyUnionTypeOrLiteral
            <|> return (Union mempty)

    nonEmptyUnionTypeOrLiteral = do
            (f, kvs) <- loop
            m <- toMap kvs
            return (f m)
          where
            loop = do
                a <- anyLabel

                let alternative0 = do
                        _equal
                        b <- expression
                        kvs <- Text.Megaparsec.many (do
                            _bar
                            c <- anyLabel
                            d <- optional (do _colon; expression)
                            return (c, d) )
                        return (UnionLit a b, kvs)

                let alternative1 = do
                        b <- optional (do _colon; expression)

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
        file <- file_ FileComponent

        return (Local Parent file)

    herePath = do
        _    <- "." :: Parser Text
        file <- file_ FileComponent

        return (Local Here file)

    homePath = do
        _    <- "~" :: Parser Text
        file <- file_ FileComponent

        return (Local Home file)

    absolutePath = do
        file <- file_ FileComponent

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

splitOn :: Text -> Text -> NonEmpty Text
splitOn needle haystack =
    case Data.Text.splitOn needle haystack of
        []     -> "" :| []
        t : ts -> t  :| ts

linesLiteral :: Chunks s a -> NonEmpty (Chunks s a)
linesLiteral (Chunks [] suffix) =
    fmap (Chunks []) (splitOn "\n" suffix)
linesLiteral (Chunks ((prefix, interpolation) : pairs₀) suffix₀) =
    foldr
        Data.List.NonEmpty.cons
        (Chunks ((lastLine, interpolation) : pairs₁) suffix₁ :| chunks)
        (fmap (Chunks []) initLines)
  where
    splitLines = splitOn "\n" prefix

    initLines = Data.List.NonEmpty.init splitLines
    lastLine  = Data.List.NonEmpty.last splitLines

    Chunks pairs₁ suffix₁ :| chunks = linesLiteral (Chunks pairs₀ suffix₀)

unlinesLiteral :: NonEmpty (Chunks s a) -> Chunks s a
unlinesLiteral chunks =
    Data.Foldable.fold (Data.List.NonEmpty.intersperse "\n" chunks)

emptyLine :: Chunks s a -> Bool
emptyLine (Chunks [] "") = True
emptyLine  _             = False

leadingSpaces :: Chunks s a -> Text
leadingSpaces chunks = Data.Text.takeWhile isSpace firstText
  where
    isSpace c = c == '\x20' || c == '\x09'

    firstText =
        case chunks of
            Chunks                []  suffix -> suffix
            Chunks ((prefix, _) : _ ) _      -> prefix

dropLiteral :: Int -> Chunks s a -> Chunks s a
dropLiteral n (Chunks [] suffix) =
    Chunks [] (Data.Text.drop n suffix)
dropLiteral n (Chunks ((prefix, interpolation) : rest) suffix) =
    Chunks ((Data.Text.drop n prefix, interpolation) : rest) suffix

toDoubleQuoted :: Chunks Src a -> Chunks Src a
toDoubleQuoted literal =
    unlinesLiteral (fmap (dropLiteral indent) literals)
  where
    literals = linesLiteral literal

    sharedPrefix ab ac =
        case Data.Text.commonPrefixes ab ac of
            Just (a, _b, _c) -> a
            Nothing          -> ""

    -- The standard specifies to filter out blank lines for all lines *except*
    -- for the last line
    filteredLines = newInit <> pure oldLast
      where
        oldInit = Data.List.NonEmpty.init literals

        oldLast = Data.List.NonEmpty.last literals

        newInit = filter (not . emptyLine) oldInit

    longestSharedPrefix =
        case filteredLines of
            l : ls ->
                Data.Foldable.foldl' sharedPrefix (leadingSpaces l) (fmap leadingSpaces ls)
            [] ->
                ""

    indent = Data.Text.length longestSharedPrefix
