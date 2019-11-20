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
import Data.Foldable (foldl')
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Dhall.Syntax
import Dhall.Src (Src(..))
import Prelude hiding (const, pi)
import Text.Parser.Combinators (choice, try, (<?>))

import qualified Control.Monad
import qualified Data.ByteArray.Encoding
import qualified Data.ByteString
import qualified Data.Char               as Char
import qualified Data.Foldable
import qualified Data.List
import qualified Data.List.NonEmpty
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Dhall.Crypto
import qualified Text.Megaparsec
#if !MIN_VERSION_megaparsec(7, 0, 0)
import qualified Text.Megaparsec.Char    as Text.Megaparsec
#endif

import Dhall.Parser.Combinators
import Dhall.Parser.Token

-- | Get the current source position
getSourcePos :: Text.Megaparsec.MonadParsec e s m =>
                m Text.Megaparsec.SourcePos
getSourcePos =
#if MIN_VERSION_megaparsec(7, 0, 0)
    Text.Megaparsec.getSourcePos
#else
    Text.Megaparsec.getPosition
#endif
{-# INLINE getSourcePos #-}

-- | Get the current source offset (in tokens)
getOffset :: Text.Megaparsec.MonadParsec e s m => m Int
#if MIN_VERSION_megaparsec(7, 0, 0)
getOffset = Text.Megaparsec.stateOffset <$> Text.Megaparsec.getParserState
#else
getOffset = Text.Megaparsec.stateTokensProcessed <$> Text.Megaparsec.getParserState
#endif
{-# INLINE getOffset #-}

-- | Set the current source offset
setOffset :: Text.Megaparsec.MonadParsec e s m => Int -> m ()
#if MIN_VERSION_megaparsec(7, 0, 0)
setOffset o = Text.Megaparsec.updateParserState $ \(Text.Megaparsec.State s _ pst) ->
  Text.Megaparsec.State s o pst
#else
setOffset o = Text.Megaparsec.updateParserState $ \(Text.Megaparsec.State s p _ stw) ->
  Text.Megaparsec.State s p o stw
#endif
{-# INLINE setOffset #-}

{-| Wrap a `Parser` to still match the same text but return only the `Src`
    span
-}
src :: Parser a -> Parser Src
src parser = do
    before      <- getSourcePos
    (tokens, _) <- Text.Megaparsec.match parser
    after       <- getSourcePos
    return (Src before after tokens)

{-| Wrap a `Parser` to still match the same text, but to wrap the resulting
    `Expr` in a `Note` constructor containing the `Src` span
-}
noted :: Parser (Expr Src a) -> Parser (Expr Src a)
noted parser = do
    before      <- getSourcePos
    (tokens, e) <- Text.Megaparsec.match parser
    after       <- getSourcePos
    let src₀ = Src before after tokens
    case e of
        Note src₁ _ | laxSrcEq src₀ src₁ -> return e
        _                                -> return (Note src₀ e)

{-| Parse a complete expression (with leading and trailing whitespace)

    This corresponds to the @complete-expression@ rule from the official
    grammar
-}
completeExpression :: Parser a -> Parser (Expr Src a)
completeExpression embedded = completeExpression_
  where
    Parsers {..} = parsers embedded

{-| Parse an \"import expression\"

    This is not the same thing as @`fmap` `Embed`@.  This parses any
    expression of the same or higher precedence as an import expression (such
    as a selector expression).  For example, this parses @(1)@

    This corresponds to the @import-expression@ rule from the official grammar
-}
importExpression :: Parser a -> Parser (Expr Src a)
importExpression embedded = importExpression_
  where
    Parsers {..} = parsers embedded

{-| For efficiency (and simplicity) we only expose two parsers from the
    result of the `parsers` function, since these are the only parsers needed
    outside of this module
-}
data Parsers a = Parsers
    { completeExpression_ :: Parser (Expr Src a)
    , importExpression_   :: Parser (Expr Src a)
    }

-- | Given a parser for imports, 
parsers :: Parser a -> Parsers a
parsers embedded = Parsers {..}
  where
    completeExpression_ = whitespace *> expression <* whitespace

    expression =
        noted
            ( choice
                [ alternative0
                , alternative1
                , alternative2
                , alternative3
                , alternative4
                , alternative5
                ]
            ) <?> "expression"
      where
        alternative0 = do
            _lambda
            whitespace
            _openParens
            whitespace
            a <- label
            whitespace
            _colon
            nonemptyWhitespace
            b <- expression
            whitespace
            _closeParens
            whitespace
            _arrow
            whitespace
            c <- expression
            return (Lam a b c)

        alternative1 = do
            _if
            nonemptyWhitespace
            a <- expression
            whitespace
            _then
            nonemptyWhitespace
            b <- expression
            whitespace
            _else
            nonemptyWhitespace
            c <- expression
            return (BoolIf a b c)

        alternative2 = do
            let binding = do
                    _let

                    src0 <- src nonemptyWhitespace

                    c <- label

                    src1 <- src whitespace

                    d <- optional (do
                        _colon

                        src2 <- src nonemptyWhitespace

                        e <- expression

                        whitespace

                        return (Just src2, e) )

                    _equal

                    src3 <- src whitespace

                    f <- expression

                    whitespace

                    return (Binding (Just src0) c (Just src1) d (Just src3) f)

            as <- Data.List.NonEmpty.some1 binding

            _in

            nonemptyWhitespace

            b <- expression

            -- 'Note's in let-in-let:
            --
            -- Subsequent @let@s that are not separated by an @in@ only get a
            -- single surrounding 'Note'. For example:
            --
            -- let x = a
            -- let y = b
            -- in  let z = c
            --     in x
            --
            -- is parsed as
            --
            -- (Note …
            --   (Let x …
            --     (Let y …
            --       (Note …
            --         (Let z …
            return (Dhall.Syntax.wrapInLets as b)

        alternative3 = do
            _forall
            whitespace
            _openParens
            whitespace
            a <- label
            whitespace
            _colon
            nonemptyWhitespace
            b <- expression
            whitespace
            _closeParens
            whitespace
            _arrow
            whitespace
            c <- expression
            return (Pi a b c)

        alternative4 = do
            _assert
            whitespace
            _colon
            nonemptyWhitespace
            a <- expression
            return (Assert a)

        alternative5 = do
            a <- operatorExpression

            let alternative4A = do
                    _arrow
                    whitespace
                    b <- expression
                    whitespace
                    return (Pi "_" a b)

            let alternative4B = do
                    _colon
                    nonemptyWhitespace
                    b <- expression
                    case shallowDenote a of
                        ListLit Nothing [] ->
                            return (ListLit (Just b) [])
                        Merge c d Nothing ->
                            return (Merge c d (Just b))
                        ToMap c Nothing ->
                            return (ToMap c (Just b))
                        _ -> return (Annot a b)

            alternative4A <|> alternative4B <|> pure a

    operatorExpression =
        foldr makeOperatorExpression applicationExpression operatorParsers

    makeOperatorExpression operatorParser subExpression =
            noted (do
                a <- subExpression

                whitespace

                b <- Text.Megaparsec.many $ do
                    op <- operatorParser

                    r  <- subExpression

                    whitespace

                    return (\l -> l `op` r)
                return (foldl' (\x f -> f x) a b))

    operatorParsers :: [Parser (Expr s a -> Expr s a -> Expr s a)]
    operatorParsers =
        [ ImportAlt    <$ _importAlt    <* nonemptyWhitespace
        , BoolOr       <$ _or           <* whitespace
        , NaturalPlus  <$ _plus         <* nonemptyWhitespace
        , TextAppend   <$ _textAppend   <* whitespace
        , ListAppend   <$ _listAppend   <* whitespace
        , BoolAnd      <$ _and          <* whitespace
        , Combine      <$ _combine      <* whitespace
        , Prefer       <$ _prefer       <* whitespace
        , CombineTypes <$ _combineTypes <* whitespace
        , NaturalTimes <$ _times        <* whitespace
        , BoolEQ       <$ _doubleEqual  <* whitespace
        , BoolNE       <$ _notEqual     <* whitespace
        , Equivalent   <$ _equivalent   <* whitespace
        ]

    applicationExpression = do
            f <-    (Some <$ _Some <* nonemptyWhitespace)
                <|> return id
            a <- noted importExpression_
            b <- Text.Megaparsec.many (try (nonemptyWhitespace *> noted importExpression_))
            return (foldl' app (f a) b)
          where
            app nL@(Note (Src before _ bytesL) _) nR@(Note (Src _ after bytesR) _) =
                Note (Src before after (bytesL <> bytesR)) (App nL nR)
            app nL nR =
                App nL nR

    importExpression_ = noted (choice [ alternative0, alternative1 ])
          where
            alternative0 = do
                a <- embedded
                return (Embed a)

            alternative1 = completionExpression

    completionExpression = noted (do
        a <- selectorExpression

        mb <- optional (do
            _doubleColon

            selectorExpression )

        case mb of
            Nothing -> return a
            Just b  -> return (RecordCompletion a b) )

    selectorExpression = noted (do
            a <- primitiveExpression

            let recordType = _openParens *> whitespace *> expression <* whitespace <* _closeParens

            let field               x  e = Field   e  x
            let projectBySet        xs e = Project e (Left  xs)
            let projectByExpression xs e = Project e (Right xs)

            let alternatives =
                        fmap field               anyLabel
                    <|> fmap projectBySet        labels
                    <|> fmap projectByExpression recordType

            b <- Text.Megaparsec.many (try (whitespace *> _dot *> whitespace *> alternatives))
            return (foldl' (\e k -> k e) a b) )

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
                    , alternative08
                    , alternative37
                    , alternative09
                    , builtin
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
                return (DoubleLit (DhallDouble b))

            alternative01 = do
                a <- try naturalLiteral
                return (NaturalLit a)

            alternative02 = do
                a <- try integerLiteral
                return (IntegerLit a)

            alternative03 = textLiteral

            alternative04 = (do
                _openBrace

                whitespace

                _ <- optional (_comma *> whitespace)

                a <- recordTypeOrLiteral

                whitespace

                _closeBrace

                return a ) <?> "literal"

            alternative05 = unionType

            alternative06 = listLiteral

            alternative07 = do
                _merge
                nonemptyWhitespace
                a <- importExpression_
                nonemptyWhitespace
                b <- importExpression_ <?> "second argument to ❰merge❱"
                return (Merge a b Nothing)

            alternative08 = do
                _toMap
                nonemptyWhitespace
                a <- importExpression_
                return (ToMap a Nothing)

            alternative09 = do
                a <- try doubleInfinity
                return (DoubleLit (DhallDouble a))

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

                let nan = DhallDouble (0.0/0.0)

                c <- Text.Megaparsec.lookAhead (Text.Megaparsec.satisfy predicate)

                case c of
                    'N' ->
                        choice
                            [ NaturalFold      <$ _NaturalFold
                            , NaturalBuild     <$ _NaturalBuild
                            , NaturalIsZero    <$ _NaturalIsZero
                            , NaturalEven      <$ _NaturalEven
                            , NaturalOdd       <$ _NaturalOdd
                            , NaturalSubtract  <$ _NaturalSubtract
                            , NaturalToInteger <$ _NaturalToInteger
                            , NaturalShow      <$ _NaturalShow
                            , Natural          <$ _Natural
                            , None             <$ _None
                            , DoubleLit nan    <$ _NaN
                            ]
                    'I' ->
                        choice
                            [ IntegerClamp     <$ _IntegerClamp
                            , IntegerNegate    <$ _IntegerNegate
                            , IntegerShow      <$ _IntegerShow
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
                whitespace
                a <- expression
                whitespace
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
                _ <- text "${"
                e <- completeExpression_
                _ <- char '}'
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
                _ <- char '$'
                return (Chunks [] "$")

            escapedCharacter = do
                _ <- char '\\'
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
                quotationMark = char '"'

                dollarSign = char '$'

                backSlash = char '\\'

                forwardSlash = char '/'

                backSpace = do _ <- char 'b'; return '\b'

                formFeed = do _ <- char 'f'; return '\f'

                lineFeed = do _ <- char 'n'; return '\n'

                carriageReturn = do _ <- char 'r'; return '\r'

                tab = do _ <- char 't'; return '\t'

                unicode = do
                    _  <- char 'u';

                    let toNumber = Data.List.foldl' (\x y -> x * 16 + y) 0

                    let fourCharacterEscapeSequence = do
                            ns <- Control.Monad.replicateM 4 hexNumber
                            
                            let number = toNumber ns

                            Control.Monad.guard (validCodepoint number)
                                <|> fail "Invalid Unicode code point"

                            return number

                    let bracedEscapeSequence = do
                            _  <- char '{'
                            ns <- some hexNumber

                            let number = toNumber ns

                            Control.Monad.guard (number <= 0x10FFFD && validCodepoint number)
                                <|> fail "Invalid Unicode code point"

                            _  <- char '}'

                            return number

                    n <- bracedEscapeSequence <|> fourCharacterEscapeSequence

                    return (Char.chr n)

    doubleQuotedLiteral = do
            _      <- char '"'
            chunks <- Text.Megaparsec.many doubleQuotedChunk
            _      <- char '"'
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
                    _ <- text "${"
                    a <- completeExpression_
                    _ <- char '}'
                    b <- singleQuoteContinue
                    return (Chunks [(mempty, a)] mempty <> b)

                escapeInterpolation = do
                    _ <- text "''${"
                    b <- singleQuoteContinue
                    return ("${" <> b)

                endLiteral = do
                    _ <- text "''"
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
                    _ <- char '\t' <?> "tab"
                    b <- singleQuoteContinue
                    return ("\t" <> b)

    singleQuoteLiteral = do
            _ <- text "''"
            _ <- endOfLine
            a <- singleQuoteContinue

            return (toDoubleQuoted a)
          where
            endOfLine = (void (char '\n') <|> void (text "\r\n")) <?> "newline"

    textLiteral = (do
            literal <- doubleQuotedLiteral <|> singleQuoteLiteral
            return (TextLit literal) ) <?> "literal"

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

            whitespace

            let nonEmptyRecordType = do
                    _colon

                    nonemptyWhitespace

                    b <- expression

                    whitespace

                    e <- Text.Megaparsec.many (do
                        _comma

                        whitespace

                        c <- anyLabel

                        whitespace

                        _colon

                        nonemptyWhitespace

                        d <- expression

                        whitespace

                        return (c, d) )

                    m <- toMap ((a, b) : e)

                    return (Record m)

            let nonEmptyRecordLiteral = do
                    _equal

                    whitespace

                    b <- expression

                    whitespace

                    e <- Text.Megaparsec.many (do
                        _comma

                        whitespace

                        c <- anyLabel

                        whitespace

                        _equal

                        whitespace

                        d <- expression

                        whitespace

                        return (c, d) )

                    m <- toMap ((a, b) : e)

                    return (RecordLit m)

            nonEmptyRecordType <|> nonEmptyRecordLiteral

    unionType = (do
            _openAngle

            whitespace

            _ <- optional (_bar *> whitespace)

            let unionTypeEntry = do
                    a <- anyLabel
                    whitespace
                    b <- optional (_colon *> nonemptyWhitespace *> expression <* whitespace)
                    return (a, b)

            kvs <- Text.Megaparsec.sepBy unionTypeEntry (_bar *> whitespace)

            m <- toMap kvs

            _closeAngle

            return (Union m) ) <?> "literal"

    listLiteral = (do
            _openBracket

            whitespace

            _ <- optional (_comma *> whitespace)

            a <- Text.Megaparsec.sepBy (expression <* whitespace) (_comma *> whitespace)

            _closeBracket

            return (ListLit Nothing (Data.Sequence.fromList a)) ) <?> "literal"

{-| Parse an environment variable import

    This corresponds to the @env@ rule from the official grammar
-}
env :: Parser ImportType
env = do
    _ <- text "env:"
    a <- (alternative0 <|> alternative1)
    return (Env a)
  where
    alternative0 = bashEnvironmentVariable

    alternative1 = do
        _ <- char '"'
        a <- posixEnvironmentVariable
        _ <- char '"'
        return a

-- | Parse a local import without trailing whitespace
localOnly :: Parser ImportType
localOnly =
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

{-| Parse a local import

    This corresponds to the @local@ rule from the official grammar
-}
local :: Parser ImportType
local = do
    a <- localOnly
    return a

{-| Parse an HTTP(S) import

    This corresponds to the @http@ rule from the official grammar
-}
http :: Parser ImportType
http = do
    url <- httpRaw
    headers <- optional (do
        try (whitespace *> _using *> nonemptyWhitespace)
        importExpression import_ )
    return (Remote (url { headers }))

{-| Parse a `Missing` import

    This corresponds to the @missing@ rule from the official grammar
-}
missing :: Parser ImportType
missing = do
  _missing
  return Missing

{-| Parse an `ImportType`

    This corresponds to the @import-type@ rule from the official grammar
-}
importType_ :: Parser ImportType
importType_ = do
    let predicate c =
            c == '~' || c == '.' || c == '/' || c == 'h' || c == 'e' || c == 'm'

    _ <- Text.Megaparsec.lookAhead (Text.Megaparsec.satisfy predicate)

    choice [ local, http, env, missing ]

{-| Parse a `Dhall.Crypto.SHA256Digest`

    This corresponds to the @hash@ rule from the official grammar
-}
importHash_ :: Parser Dhall.Crypto.SHA256Digest
importHash_ = do
    _ <- text "sha256:"
    t <- count 64 (satisfy hexdig <?> "hex digit")
    let strictBytes16 = Data.Text.Encoding.encodeUtf8 t
    strictBytes <- case Data.ByteArray.Encoding.convertFromBase Base16 strictBytes16 of
        Left  string      -> fail string
        Right strictBytes -> return (strictBytes :: Data.ByteString.ByteString)
    case Dhall.Crypto.sha256DigestFromByteString strictBytes of
      Nothing -> fail "Invalid sha256 hash"
      Just h  -> pure h

{-| Parse an `ImportHashed`

    This corresponds to the @import-hashed@ rule from the official grammar
-}
importHashed_ :: Parser ImportHashed
importHashed_ = do
    importType <- importType_
    hash       <- optional (try (nonemptyWhitespace *> importHash_))
    return (ImportHashed {..})

{-| Parse an `Import`

    This corresponds to the @import@ rule from the official grammar
-}
import_ :: Parser Import
import_ = (do
    importHashed <- importHashed_
    importMode   <- alternative <|> pure Code
    return (Import {..}) ) <?> "import"
  where
    alternative = do
      try (whitespace *> _as *> nonemptyWhitespace)

      (_Text >> pure RawText) <|> (_Location >> pure Location)

-- | Same as @Data.Text.splitOn@, except always returning a `NonEmpty` result
splitOn :: Text -> Text -> NonEmpty Text
splitOn needle haystack =
    case Data.Text.splitOn needle haystack of
        []     -> "" :| []
        t : ts -> t  :| ts

-- | Split `Chunks` by lines
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

-- | Flatten several `Chunks` back into a single `Chunks` by inserting newlines
unlinesLiteral :: NonEmpty (Chunks s a) -> Chunks s a
unlinesLiteral chunks =
    Data.Foldable.fold (Data.List.NonEmpty.intersperse "\n" chunks)

-- | Returns `True` if the `Chunks` represents a blank line
emptyLine :: Chunks s a -> Bool
emptyLine (Chunks [] ""  ) = True
emptyLine (Chunks [] "\r") = True  -- So that `\r\n` is treated as a blank line
emptyLine  _               = False

-- | Return the leading whitespace for a `Chunks` literal
leadingSpaces :: Chunks s a -> Text
leadingSpaces chunks = Data.Text.takeWhile isSpace firstText
  where
    isSpace c = c == '\x20' || c == '\x09'

    firstText =
        case chunks of
            Chunks                []  suffix -> suffix
            Chunks ((prefix, _) : _ ) _      -> prefix

-- | Drop the first @n@ characters for a `Chunks` literal
dropLiteral :: Int -> Chunks s a -> Chunks s a
dropLiteral n (Chunks [] suffix) =
    Chunks [] (Data.Text.drop n suffix)
dropLiteral n (Chunks ((prefix, interpolation) : rest) suffix) =
    Chunks ((Data.Text.drop n prefix, interpolation) : rest) suffix

{-| Convert a single-quoted `Chunks` literal to the equivalent double-quoted
    `Chunks` literal
-}
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
