{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}

-- | Dhall grammar parsed from an Alex token stream.
module Dhall.Parser.Grammar
    ( completeExpression
    , importExpression
    , parseFile
    , Parsers(..)
    , parsers
    ) where

import Control.Applicative     (Alternative (..), optional)
import Control.Monad           (unless, void, when)
import Data.Foldable           (foldl')
import Data.Functor            (($>))
import Data.List.NonEmpty      (NonEmpty (..))
import Data.Text               (Text)
import Dhall.Src               (Src (..))
import Dhall.Syntax
import Numeric.Natural         (Natural)
import Text.Megaparsec         (try, (<?>))
import Text.Parser.Combinators (choice)

import Dhall.Parser.Lex
import Dhall.Parser.TokenType

import qualified Control.Monad.Combinators          as Combinators
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Data.Char                          as Char
import qualified Data.List.NonEmpty                 as NonEmpty
import qualified Data.Sequence
import qualified Data.Text                          as Text
import qualified Dhall.Crypto
import qualified Dhall.Map
import qualified Dhall.Parser.Expression            as CharExpr
import qualified Dhall.Parser.Token                 as CharToken
import qualified Text.Megaparsec                    as Megaparsec

import {-# SOURCE #-} Dhall.Pretty.Internal (ChooseCharacterSet(..))

-- | Parse a complete expression (leading and trailing trivia).
completeExpression :: TParser a -> TParser (Expr Src a)
completeExpression embedded = completeExpression_
  where
    Parsers {..} = parsers embedded

-- | Parse an import-expression.
importExpression :: TParser a -> TParser (Expr Src a)
importExpression embedded = importExpression_
  where
    Parsers {..} = parsers embedded

-- | Header text plus the expression, requiring EOF.
parseFile :: TParser (Text, Expr Src Import)
parseFile = do
    stream <- Megaparsec.getInput
    (headerToks, _) <- Megaparsec.match (Megaparsec.many (satisfyKind isTrivia))
    let header = tokensText stream headerToks
    e <- completeExpression importP
    whitespace
    Megaparsec.eof
    return (header, e)

importP :: TParser Import
importP = import_ <?> "import"

data Parsers a = Parsers
    { completeExpression_ :: TParser (Expr Src a)
    , importExpression_   :: TParser (Expr Src a)
    , letBinding          :: TParser (Binding Src a)
    }

kindEq :: TokKind -> TParser ()
kindEq k = void (kind_ k)

requireWhsp1 :: String -> TParser ()
requireWhsp1 after =
    nonemptyWhitespace <|> fail ("Whitespace is required after " <> after)

expectClose :: TokKind -> String -> String -> TParser ()
expectClose closer extraMsg missingMsg = do
    whitespace
    atClose <- (True <$ Megaparsec.lookAhead (kindEq closer)) <|> pure False
    atComma <- (True <$ Megaparsec.lookAhead (kindEq TkComma)) <|> pure False
    case (atClose, atComma) of
        (True, _) -> return ()
        (_, True) -> fail extraMsg
        _         -> fail missingMsg

expectCloseBrace :: String -> String -> TParser ()
expectCloseBrace extraMsg missingMsg =
    expectClose TkBraceR extraMsg missingMsg

keywordAsLabelName :: TokKind -> Maybe Text
keywordAsLabelName k = case k of
    TkIf              -> Just "if"
    TkThen            -> Just "then"
    TkElse            -> Just "else"
    TkLet             -> Just "let"
    TkIn              -> Just "in"
    TkAs              -> Just "as"
    TkUsing           -> Just "using"
    TkMerge           -> Just "merge"
    TkToMap           -> Just "toMap"
    TkShowConstructor -> Just "showConstructor"
    TkAssert          -> Just "assert"
    TkWith            -> Just "with"
    TkMissing         -> Just "missing"
    _                 -> Nothing

anyLabelOrSomeOrKeywordHint :: TParser Text
anyLabelOrSomeOrKeywordHint = do
    k <- peekKind
    case keywordAsLabelName k of
        Just name ->
            fail
                (  "Keyword "
                <> Text.unpack name
                <> " cannot be used as a label; quote it as `"
                <> Text.unpack name
                <> "`"
                )
        Nothing -> anyLabelOrSome

startsRecordFieldLabel :: TokKind -> Bool
startsRecordFieldLabel k =
    case k of
        TkIdent           -> True
        TkQuotedLabel     -> True
        TkBuiltin         -> True
        TkSome            -> True
        TkShowConstructor -> True
        _                 -> keywordAsLabelName k /= Nothing

peekRecordFieldStart :: TParser ()
peekRecordFieldStart =
    Megaparsec.lookAhead $ do
        whitespace
        void (satisfyKind startsRecordFieldLabel)

_lambda :: TParser CharacterSet
_lambda = do
    t <- satisfyKind (\k -> case k of TkLambda _ -> True; _ -> False)
    case tokKind t of
        TkLambda cs -> return cs
        _           -> empty

_arrow :: TParser CharacterSet
_arrow = do
    t <- satisfyKind (\k -> case k of TkArrow _ -> True; _ -> False)
    case tokKind t of
        TkArrow cs -> return cs
        _          -> empty

_forall :: TParser CharacterSet
_forall = do
    t <- satisfyKind (\k -> case k of TkForall _ -> True; _ -> False)
    case tokKind t of
        TkForall cs -> return cs
        _           -> empty

_equivalent :: TParser CharacterSet
_equivalent = do
    t <- satisfyKind (\k -> case k of TkEquiv _ -> True; _ -> False)
    case tokKind t of
        TkEquiv cs -> return cs
        _          -> empty

_combine :: TParser CharacterSet
_combine = do
    t <- satisfyKind (\k -> case k of TkCombine _ -> True; _ -> False)
    case tokKind t of
        TkCombine cs -> return cs
        _            -> empty

_prefer :: TParser CharacterSet
_prefer = do
    t <- satisfyKind (\k -> case k of TkPrefer _ -> True; _ -> False)
    case tokKind t of
        TkPrefer cs -> return cs
        _           -> empty

_combineTypes :: TParser CharacterSet
_combineTypes = do
    t <- satisfyKind (\k -> case k of TkCombineTypes _ -> True; _ -> False)
    case tokKind t of
        TkCombineTypes cs -> return cs
        _                 -> empty

labelText :: TParser Text
labelText = do
    t <- satisfyKind $ \k -> case k of
        TkQuotedLabel    -> True
        TkIdent          -> True
        TkShowConstructor -> True
        _                -> False
    case tokKind t of
        TkQuotedLabel     -> return (quotedLabelInner t)
        TkIdent           -> return (tokText t)
        TkShowConstructor -> return "showConstructor"
        _                 -> empty

anyLabelText :: TParser Text
anyLabelText = labelText <|> builtinName
  where
    builtinName = do
        t <- satisfyKind (== TkBuiltin)
        return (tokText t)

label :: TParser Text
label = labelText <?> "label"

anyLabel :: TParser Text
anyLabel = anyLabelText <?> "any label"

anyLabelOrSome :: TParser Text
anyLabelOrSome = anyLabel <|> (kindEq TkSome $> "Some")

natural :: TParser Natural
natural = do
    t <- satisfyKind $ \k -> case k of TkNatural _ -> True; _ -> False
    case tokKind t of
        TkNatural n -> return n
        _           -> empty

identifier :: TParser Var
identifier = do
    x <- label
    n <- optional $ try $ do
        whitespace
        kindEq TkAt
        whitespace
        natural
    return (V x (maybe 0 fromIntegral n))

labels :: TParser [Text]
labels = do
    kindEq TkBraceL
    whitespace
    nonEmptyLabels <|> emptyLabels
  where
    emptyLabels = do
        _ <- optional (kindEq TkComma *> whitespace)
        kindEq TkBraceR
        return []
    nonEmptyLabels = do
        _ <- optional (kindEq TkComma *> whitespace)
        x <- anyLabelOrSome
        whitespace
        xs <- many $ try $ do
            kindEq TkComma
            whitespace
            Megaparsec.notFollowedBy (kindEq TkBraceR)
            l <- anyLabelOrSome
            whitespace
            return l
        _ <- optional (kindEq TkComma *> whitespace)
        kindEq TkBraceR
        return (x : xs)

parsers :: forall a. TParser a -> Parsers a
parsers embedded = Parsers{..}
  where
    typedBinder = do
        src0 <- src whitespace
        k <- peekKind
        when (k == TkBraceL) $
            fail "Binders require a variable name before the type, not a record pattern"
        when (k == TkColon) $
            fail "Missing binder variable name"
        when (k == TkParenR) $
            fail "Missing binder variable name"
        a <- label <?> "binder variable name"
        src1 <- src whitespace
        kindEq TkColon
        src2 <- src (requireWhsp1 ":")
        b <- expression
        whitespace
        kindEq TkParenR
        return (src0, a, src1, src2, b)

    completeExpression_ =
            whitespace
        *>  expression
        <*  whitespace

    letBinding = do
        src0 <- try (kindEq TkLet *> src nonemptyWhitespace)

        c <- label

        src1 <- src whitespace

        d <- optional (do
            kindEq TkColon
            src2 <- src (requireWhsp1 ":")
            e <- expression
            whitespace
            return (Just src2, e) )

        kindEq TkEqual

        src3 <- src whitespace

        f <- expression

        whitespace

        return (Binding (Just src0) c (Just src1) d (Just src3) f)

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
            cs <- _lambda
            whitespace
            kindEq TkParenL
            (src0, a, src1, src2, b) <- typedBinder
            whitespace
            cs' <- _arrow
            whitespace
            c <- expression
            return (Lam (Specify (cs <> cs')) (FunctionBinding (Just src0) a (Just src1) (Just src2) b) c)

        alternative1 = do
            try (kindEq TkIf *> nonemptyWhitespace)
            a <- expression
            whitespace
            try (kindEq TkThen *> nonemptyWhitespace)
            b <- expression
            whitespace
            try (kindEq TkElse *> nonemptyWhitespace)
            c <- expression
            return (BoolIf a b c)

        alternative2 = do
            as <- Combinators.NonEmpty.some letBinding
            try (kindEq TkIn *> nonemptyWhitespace)
            b <- expression
            return (Dhall.Syntax.wrapInLets as b)

        alternative3 = do
            cs <- try (_forall <* whitespace <* kindEq TkParenL)
            (_, a, _, _, b) <- typedBinder
            whitespace
            cs' <- _arrow
            whitespace
            c <- expression
            return (Pi (Specify (cs <> cs')) a b c)

        alternative4 = do
            try (kindEq TkAssert *> whitespace *> kindEq TkColon)
            requireWhsp1 ":"
            a <- expression
            return (Assert a)

        alternative5 = do
            (a0Info, a0) <- applicationExpressionWithInfo

            let (parseFirstOperatorExpression, parseOperatorExpression) =
                    operatorExpression (pure a0)

            let alternative5A = do
                    case a0Info of
                        ImportExpr -> return ()
                        _          -> empty

                    bs <- some (do
                        try (nonemptyWhitespace *> kindEq TkWith *> nonemptyWhitespace)

                        let withComponent =
                                    fmap WithLabel anyLabelOrSome
                                <|> fmap (\_ -> WithQuestion) (kindEq TkImportAlt)

                        keys <- Combinators.NonEmpty.sepBy1 withComponent
                            (try (whitespace *> kindEq TkDot) *> whitespace)

                        whitespace
                        kindEq TkEqual
                        whitespace
                        value <- parseOperatorExpression
                        return (\e -> With e keys value) )

                    return (foldl' (\e f -> f e) a0 bs)

            let alternative5B = do
                    a <- parseFirstOperatorExpression
                    whitespace

                    let alternative5B0 = do
                            cs <- _arrow
                            whitespace
                            b <- expression
                            whitespace
                            return (Pi (Specify cs) "_" a b)

                    let alternative5B1 = do
                            kindEq TkColon
                            requireWhsp1 ":"
                            case (shallowDenote a, a0Info) of
                                (ListLit Nothing [], _) -> do
                                    b <- expression
                                    return (ListLit (Just b) [])
                                (Merge c d Nothing, NakedMergeOrSomeOrToMap) -> do
                                    b <- expression
                                    return (Merge c d (Just b))
                                (ToMap c Nothing, NakedMergeOrSomeOrToMap) -> do
                                    b <- expression
                                    return (ToMap c (Just b))
                                _ -> do
                                    b <- expression
                                    return (Annot a b)

                    let alternative5B2 =
                            case shallowDenote a of
                                ListLit Nothing [] ->
                                    fail "Empty list literal without annotation"
                                _ -> pure a

                    alternative5B0 <|> alternative5B1 <|> alternative5B2

            alternative5A <|> alternative5B

    operatorExpression firstApplicationExpression =
        foldr cons nil operatorParsers
      where
        cons operatorParser (p0, p) =
            ( makeOperatorExpression p0 operatorParser p
            , makeOperatorExpression p  operatorParser p
            )
        nil = (firstApplicationExpression, applicationExpression)

    makeOperatorExpression firstSubExpression operatorParser subExpression = do
            stream <- Megaparsec.getInput
            let srcBuf = tsSource stream
            let startOff = currentOff stream
            a <- firstSubExpression
            -- After a subexpression the next token is often a delimiter.
            -- `try` is required so trailing trivia plus EOF is an empty
            -- failure that `optional` can turn into Nothing.
            mk <- Megaparsec.lookAhead $ optional $ try $ do
                whitespace
                tokKind <$> Megaparsec.anySingle
            case mk of
                Just k | isOperatorKind k -> parseOperators srcBuf startOff a
                _                         -> return a
          where
            parseOperators srcBuf startOff a = do
                bs <- Megaparsec.many $ do
                    op0 <- try (whitespace *> operatorParser)
                    r0 <- subExpression
                    endOff <- currentOff <$> Megaparsec.getInput
                    let l@(Note (Src startL _ _) _) `op` r@(Note (Src _ endR _) _) =
                            Note (Src startL endR (spanText srcBuf startOff endOff)) (l `op0` r)
                        l `op` r = l `op0` r
                    return (`op` r0)
                return (foldl' (\x f -> f x) a bs)

            isOperatorKind k = case k of
                TkEquiv _        -> True
                TkImportAlt      -> True
                TkOr             -> True
                TkPlus           -> True
                TkTextAppend     -> True
                TkListAppend     -> True
                TkAnd            -> True
                TkCombine _      -> True
                TkPrefer _       -> True
                TkCombineTypes _ -> True
                TkTimes          -> True
                TkEQ             -> True
                TkNE             -> True
                _                -> False

    operatorParsers :: [TParser (Expr s a -> Expr s a -> Expr s a)]
    operatorParsers =
        [ Equivalent . Specify        <$> _equivalent   <* whitespace
        , ImportAlt                   <$  kindEq TkImportAlt <* nonemptyWhitespace
        , BoolOr                      <$  kindEq TkOr        <* whitespace
        , NaturalPlus                 <$  kindEq TkPlus      <* nonemptyWhitespace
        , TextAppend                  <$  kindEq TkTextAppend <* whitespace
        , ListAppend                  <$  kindEq TkListAppend <* whitespace
        , BoolAnd                     <$  kindEq TkAnd        <* whitespace
        , (\cs -> Combine (Specify cs) Nothing)         <$> _combine <* whitespace
        , (\cs -> Prefer (Specify cs) PreferFromSource) <$> _prefer  <* whitespace
        , CombineTypes . Specify      <$> _combineTypes <* whitespace
        , NaturalTimes                <$  kindEq TkTimes <* whitespace
        , BoolEQ                      <$  kindEq TkEQ    <* whitespace
        , BoolNE                      <$  kindEq TkNE    <* whitespace
        ]

    applicationExpression = snd <$> applicationExpressionWithInfo

    applicationExpressionWithInfo :: TParser (ApplicationExprInfo, Expr Src a)
    applicationExpressionWithInfo = do
            let alternative0 = do
                    try (kindEq TkMerge *> nonemptyWhitespace)
                    a <-
                        importExpression_
                            <* (nonemptyWhitespace <?> "second argument to ❰merge❱")
                    return (\b -> Merge a b Nothing, Just "second argument to ❰merge❱")

            let alternative1 = do
                    kindEq TkSome
                    hasArg <- (True <$ nonemptyWhitespace) <|> pure False
                    unless hasArg Megaparsec.eof
                    when (not hasArg) $
                        fail "argument to ❰Some❱"
                    k <- peekKind
                    when (k == TkColon) $
                        fail
                            "Some is a constructor and cannot be annotated like a type; write Some <value> : Optional T"
                    return (Some, Just "argument to ❰Some❱")

            let alternative2 = do
                    try (kindEq TkToMap *> nonemptyWhitespace)
                    return (\a -> ToMap a Nothing, Just "argument to ❰toMap❱")

            let alternative3 = do
                    try (kindEq TkShowConstructor *> nonemptyWhitespace)
                    return (\a -> ShowConstructor a, Just "argument to ❰showConstructor❱")

            let alternative4 = return (id, Nothing)

            (f, maybeMessage) <-
                alternative0 <|> alternative1 <|> alternative2 <|> alternative3 <|> alternative4

            let adapt parser =
                    case maybeMessage of
                        Nothing      -> parser
                        Just message -> parser <?> message

            stream <- Megaparsec.getInput
            let srcBuf = tsSource stream
            let startOff = currentOff stream

            a <- adapt (noted importExpression_)

            bs <- Megaparsec.many $ do
                try (nonemptyWhitespace <* Megaparsec.lookAhead (satisfyKind startsImportExpression))
                b <- importExpression_
                endOff <- currentOff <$> Megaparsec.getInput
                return (endOff, b)

            let c = foldl' (app srcBuf startOff) (f a) bs

            let info =
                    case (maybeMessage, bs) of
                        (Just _ , []) -> NakedMergeOrSomeOrToMap
                        (Nothing, []) -> ImportExpr
                        _             -> ApplicationExpr

            return (info, c)
          where
            app srcBuf startOff lhs (endOff, b)
                | Note (Src left _ _) _ <- lhs
                , Note (Src _ right _) _ <- b
                = Note (Src left right (spanText srcBuf startOff endOff)) (App lhs b)
            app _ _ lhs (_, b) =
                App lhs b

    importExpression_ = noted (choice [ alternative0, alternative1 ])
          where
            alternative0 = Embed <$> embedded
            alternative1 = completionExpression

    completionExpression = noted (do
        a <- selectorExpression
        mb <- optional (do
            try (whitespace *> kindEq TkDoubleColon)
            whitespace
            selectorExpression)
        case mb of
            Nothing -> return a
            Just b  -> return (RecordCompletion a b) )

    selectorExpression = noted (do
            a <- primitiveExpression

            let recordType = kindEq TkParenL *> whitespace *> expression <* whitespace <* kindEq TkParenR

            let field               x  e = Field   e  x
            let projectBySet        xs e = Project e (Left  xs)
            let projectByExpression xs e = Project e (Right xs)

            let alternatives = do
                    src0 <- src whitespace
                    let fieldSelection = do
                            l <- anyLabel
                            pos <- Megaparsec.getSourcePos
                            let src1 = Src pos pos ""
                            return (FieldSelection (Just src0) l (Just src1))
                    let result =
                                fmap field               fieldSelection
                            <|> fmap projectBySet        labels
                            <|> fmap projectByExpression recordType
                    result

            b <- Megaparsec.many $ try $ do
                whitespace
                kindEq TkDot
                _ <- Megaparsec.lookAhead (whitespace *> Megaparsec.anySingle)
                alternatives

            return (foldl' (\e k -> k e) a b) )

    primitiveExpression =
            noted
                ( do
                    mk <- Megaparsec.lookAhead (optional (tokKind <$> Megaparsec.anySingle))
                    case mk of
                        Just (TkBytes _)     -> bytesLiteral
                        Just TkTemporal      -> temporalLiteral
                        Just (TkDouble _)    -> alternative00
                        Just (TkNatural _)   -> alternative01
                        Just (TkInteger _)   -> alternative02
                        Just TkDQuote        -> textLiteral
                        Just TkSQuoteBegin   -> textLiteral
                        Just TkBraceL        -> alternative04
                        Just TkAngleL        -> unionType
                        Just TkBrackL        -> listLiteral
                        Just TkIdent         -> alternative37
                        Just TkQuotedLabel   -> alternative37
                        Just TkShowConstructor -> alternative37
                        Just (TkInfinity _)  -> alternative09
                        Just TkBuiltin       -> builtin
                        Just TkNaN           -> builtin
                        Just TkParenL        -> empty
                        _                    -> originalChoice
                )
            <|> alternative38
          where
            originalChoice =
                choice
                    [ bytesLiteral
                    , temporalLiteral
                    , alternative00
                    , alternative01
                    , alternative02
                    , textLiteral
                    , alternative04
                    , unionType
                    , listLiteral
                    , alternative37
                    , alternative09
                    , builtin
                    ]

            alternative00 = do
                t <- satisfyKind $ \k -> case k of TkDouble _ -> True; _ -> False
                case tokKind t of
                    TkDouble a
                        | isInfinite a -> fail "double out of bounds"
                        | otherwise    -> return (DoubleLit (DhallDouble a))
                    _ -> empty

            alternative01 = do
                n <- natural
                return (NaturalLit n)

            alternative02 = do
                t <- satisfyKind $ \k -> case k of TkInteger _ -> True; _ -> False
                case tokKind t of
                    TkInteger a -> return (IntegerLit a)
                    _           -> empty

            alternative04 = (do
                kindEq TkBraceL
                src0 <- src whitespace
                mComma <- optional (kindEq TkComma)
                src1 <- case mComma of
                    Nothing -> return src0
                    Just _  -> src whitespace
                a <- recordTypeOrLiteral src1
                kindEq TkBraceR
                return a ) <?> "literal"

            alternative09 = do
                t <- satisfyKind $ \k -> case k of TkInfinity _ -> True; _ -> False
                case tokKind t of
                    TkInfinity True  -> return (DoubleLit (DhallDouble (-1/0)))
                    TkInfinity False -> return (DoubleLit (DhallDouble (1/0)))
                    _                -> empty

            builtin = do
                t <- satisfyKind $ \k -> case k of
                    TkBuiltin -> True
                    TkNaN     -> True
                    _         -> False
                let nan = DhallDouble (0.0/0.0)
                case tokKind t of
                    TkNaN -> return (DoubleLit nan)
                    TkBuiltin -> case tokText t of
                        "Natural/fold"      -> return NaturalFold
                        "Natural/build"     -> return NaturalBuild
                        "Natural/isZero"    -> return NaturalIsZero
                        "Natural/even"      -> return NaturalEven
                        "Natural/odd"       -> return NaturalOdd
                        "Natural/subtract"  -> return NaturalSubtract
                        "Natural/toInteger" -> return NaturalToInteger
                        "Natural/show"      -> return NaturalShow
                        "Natural"           -> return Natural
                        "None"              -> return None
                        "Integer/clamp"     -> return IntegerClamp
                        "Integer/negate"    -> return IntegerNegate
                        "Integer/show"      -> return IntegerShow
                        "Integer/toDouble"  -> return IntegerToDouble
                        "Integer"           -> return Integer
                        "Date/show"         -> return DateShow
                        "Date"              -> return Date
                        "Double/show"       -> return DoubleShow
                        "Double"            -> return Double
                        "List/build"        -> return ListBuild
                        "List/fold"         -> return ListFold
                        "List/length"       -> return ListLength
                        "List/head"         -> return ListHead
                        "List/last"         -> return ListLast
                        "List/indexed"      -> return ListIndexed
                        "List/reverse"      -> return ListReverse
                        "List"              -> return List
                        "Optional"          -> return Optional
                        "Bool"              -> return Bool
                        "Bytes"             -> return Bytes
                        "Sort"              -> return (Const Sort)
                        "Text/replace"      -> return TextReplace
                        "Text/show"         -> return TextShow
                        "Text"              -> return Text
                        "TimeZone/show"     -> return TimeZoneShow
                        "TimeZone"          -> return TimeZone
                        "Time/show"         -> return TimeShow
                        "Time"              -> return Time
                        "True"              -> return (BoolLit True)
                        "Type"              -> return (Const Type)
                        "False"             -> return (BoolLit False)
                        "Kind"              -> return (Const Kind)
                        _                   -> empty
                    _ -> empty

            alternative37 = do
                a <- identifier
                return (Var a)

            alternative38 = do
                kindEq TkParenL
                whitespace
                a <- expression
                whitespace
                kindEq TkParenR
                return a

    bytesLiteral = do
        t <- satisfyKind $ \k -> case k of TkBytes _ -> True; _ -> False
        case tokKind t of
            TkBytes bs -> return (BytesLit bs)
            _          -> empty

    temporalLiteral = do
        t <- satisfyKind (== TkTemporal)
        runSnippet CharExpr.temporalLiteral (tokText t)

    textLiteral = (do
        literal <- doubleQuotedLiteral <|> singleQuoteLiteral
        return (TextLit literal) ) <?> "literal"

    doubleQuotedLiteral = do
        kindEq TkDQuote
        chunks <- Megaparsec.many doubleQuotedChunk
        kindEq TkDQuote
        return (mconcat chunks)

    doubleQuotedChunk = interpolation <|> stringChunk unescapeDouble
      where
        interpolation = do
            kindEq TkInterpOpen
            e <- completeExpression_
            kindEq TkInterpClose
            return (Chunks [(mempty, e)] mempty)

    singleQuoteLiteral = do
        kindEq TkSQuoteBegin
        a <- singleQuoteChunks
        return (Dhall.Syntax.toDoubleQuoted a)

    singleQuoteChunks = do
        t <- Megaparsec.anySingle
        case tokKind t of
            TkSQuoteEnd -> return mempty
            TkInterpOpen -> do
                e <- completeExpression_
                kindEq TkInterpClose
                rest <- singleQuoteChunks
                return (Chunks [(mempty, e)] mempty <> rest)
            TkStringChunk -> do
                rest <- singleQuoteChunks
                return (Chunks [] (unescapeSingle (tokText t)) <> rest)
            _ -> fail "unexpected token in single-quoted string"

    stringChunk unescape = do
        t <- satisfyKind (== TkStringChunk)
        return (Chunks [] (unescape (tokText t)))

    recordTypeOrLiteral firstSrc0 =
            choice
                [ emptyRecordLiteral
                , nonEmptyRecordTypeOrLiteral firstSrc0
                , emptyRecordType
                ]

    emptyRecordLiteral = do
        kindEq TkEqual
        _ <- optional (try (whitespace *> kindEq TkComma))
        whitespace
        return (RecordLit mempty)

    emptyRecordType =
        Megaparsec.lookAhead (kindEq TkBraceR) *> return (Record mempty)

    nonEmptyRecordTypeOrLiteral firstSrc0 = do
            let nonEmptyRecordType = do
                    (firstKeySrc1, a) <- try $ do
                        a <- anyLabelOrSome
                        s <- src whitespace
                        kindEq TkColon
                        return (s, a)
                    firstKeySrc2 <- src (requireWhsp1 ":")
                    b <- expression
                    e <- Megaparsec.many $ try $ do
                        kindEq TkComma
                        src0' <- src whitespace
                        Megaparsec.notFollowedBy (kindEq TkBraceR)
                        c <- anyLabelOrSome
                        src1 <- src whitespace
                        kindEq TkColon
                        src2 <- src (requireWhsp1 ":")
                        d <- expression
                        whitespace
                        return (c, RecordField (Just src0') d (Just src1) (Just src2))
                    _ <- optional (whitespace *> kindEq TkComma)
                    whitespace
                    expectCloseBrace
                        "Unexpected extra ',' in record type"
                        "Missing ',' in record type"
                    m <- tToMap
                        ((a, RecordField (Just firstSrc0) b (Just firstKeySrc1) (Just firstKeySrc2)) : e)
                    return (Record m)

            let keysValue maybeSrc = do
                    firstSrc0' <- case maybeSrc of
                        Just src0 -> return src0
                        Nothing   -> src whitespace
                    firstLabel <- anyLabelOrSomeOrKeywordHint
                    firstSrc1 <- src whitespace
                    let parseLabelWithWhsp = try $ do
                            kindEq TkDot
                            src0 <- src whitespace
                            l <- anyLabelOrSome
                            src1 <- src whitespace
                            return (src0, l, src1)
                    restKeys <- Combinators.many parseLabelWithWhsp
                    let keys = (firstSrc0', firstLabel, firstSrc1) :| restKeys
                    let normalRecordEntry = do
                            try (kindEq TkEqual)
                            lastSrc2 <- src whitespace
                            value <- expression
                            let cons (s0, key, s1) (key', values) =
                                    (key, RecordField (Just s0) (RecordLit [ (key', values) ]) (Just s1) Nothing)
                            let (lastSrc0, lastLabel, lastSrc1) = NonEmpty.last keys
                            let nil = (lastLabel, RecordField (Just lastSrc0) value (Just lastSrc1) (Just lastSrc2))
                            return (foldr cons nil (NonEmpty.init keys))
                    let punnedEntry =
                            case keys of
                                (s0, x, s1) :| [] ->
                                    return (x, RecordField (Just s0) (Var (V x 0)) (Just s1) Nothing)
                                _ -> empty
                    atColon <- (True <$ Megaparsec.lookAhead (kindEq TkColon)) <|> pure False
                    when atColon $
                        fail
                            "Record literals use '=' for field values, not ':'; ':' starts a record type"
                    (normalRecordEntry <|> punnedEntry) <* whitespace

            let nonEmptyRecordLiteral = do
                    a <- keysValue (Just firstSrc0)
                    as <- many $ do
                        try (kindEq TkComma <* peekRecordFieldStart)
                        keysValue Nothing
                    _ <- optional (whitespace *> kindEq TkComma)
                    whitespace
                    expectCloseBrace
                        "Unexpected extra ',' in record literal"
                        "Missing ',' in record literal"
                    let combine k = liftA2 $ \rf rf' -> makeRecordField $ Combine mempty (Just k)
                                                            (recordFieldValue rf')
                                                            (recordFieldValue rf)
                    m <- tToMapWith combine (a : as)
                    return (RecordLit m)

            nonEmptyRecordType <|> nonEmptyRecordLiteral

    unionType = (do
            kindEq TkAngleL
            whitespace
            let unionTypeEntry = do
                    a <- anyLabelOrSome
                    whitespace
                    b <- optional (kindEq TkColon *> requireWhsp1 ":" *> expression <* whitespace)
                    return (a, b)
            let nonEmptyUnionType = do
                    kv <- try (optional (kindEq TkBar *> whitespace) *> unionTypeEntry)
                    kvs <- many (try (kindEq TkBar *> whitespace *> unionTypeEntry))
                    m <- tToMap (kv : kvs)
                    _ <- optional (kindEq TkBar *> whitespace)
                    kindEq TkAngleR
                    return (Union m)
            let emptyUnionType = do
                    try (optional (kindEq TkBar *> whitespace) *> kindEq TkAngleR)
                    return (Union mempty)
            nonEmptyUnionType <|> emptyUnionType ) <?> "literal"

    listLiteral = (do
            kindEq TkBrackL
            whitespace
            _ <- optional (kindEq TkComma *> whitespace)
            let emptyListLiteral = do
                    kindEq TkBrackR
                    return (ListLit Nothing mempty)
            let nonEmptyListLiteral = do
                    a <- expression
                    whitespace
                    as <- many $ do
                        try (kindEq TkComma *> whitespace <* Megaparsec.notFollowedBy (kindEq TkBrackR))
                        e <- expression
                        whitespace
                        return e
                    _ <- optional (kindEq TkComma *> whitespace)
                    expectClose
                        TkBrackR
                        "Unexpected extra ',' in list literal"
                        "Missing ',' in list literal"
                    kindEq TkBrackR
                    return (ListLit Nothing (Data.Sequence.fromList (a : as)))
            emptyListLiteral <|> nonEmptyListLiteral) <?> "literal"

import_ :: TParser Import
import_ = (do
    importHashed <- importHashed_
    importMode   <- alternative <|> pure Code
    return (Import {..}) ) <?> "import"
  where
    alternative = do
        try (whitespace *> kindEq TkAs *> nonemptyWhitespace)
        (void (Megaparsec.satisfy (isBuiltin "Text")) $> RawText)
            <|> (void (Megaparsec.satisfy (isIdent "Location")) $> Location)
            <|> (void (Megaparsec.satisfy (isBuiltin "Bytes")) $> RawBytes)

importHashed_ :: TParser ImportHashed
importHashed_ = do
    importType <- importType_
    hash       <- optional (try (nonemptyWhitespace *> importHash_))
    return (ImportHashed {..})

importType_ :: TParser ImportType
importType_ = choice [ local, http, env, missing ]

missing :: TParser ImportType
missing = kindEq TkMissing $> Missing

local :: TParser ImportType
local = do
    t <- satisfyKind (== TkPath)
    runSnippet CharExpr.localOnly (tokText t)

http :: TParser ImportType
http = do
    t <- satisfyKind (== TkHttpRaw)
    url <- runSnippet CharToken.httpRaw (tokText t)
    headers <- optional (do
        try (whitespace *> kindEq TkUsing *> nonemptyWhitespace)
        importExpression importP)
    return (Remote (url { headers }))

env :: TParser ImportType
env = do
    t <- satisfyKind (== TkEnv)
    runSnippet CharExpr.env (tokText t)

importHash_ :: TParser Dhall.Crypto.SHA256Digest
importHash_ = do
    t <- satisfyKind (== TkHash)
    runSnippet CharExpr.importHash_ (tokText t)

quotedLabelInner :: Tok -> Text
quotedLabelInner t =
    let txt = tokText t
    in  Text.take (max 0 (Text.length txt - 2)) (Text.drop 1 txt)

isBuiltin :: Text -> Tok -> Bool
isBuiltin n t = tokKind t == TkBuiltin && tokText t == n

isIdent :: Text -> Tok -> Bool
isIdent n t = tokKind t == TkIdent && tokText t == n

unescapeDouble :: Text -> Text
unescapeDouble t
    | t == "\\n"  = "\n"
    | t == "\\t"  = "\t"
    | t == "\\r"  = "\r"
    | t == "\\b"  = "\b"
    | t == "\\f"  = "\f"
    | t == "\\/"  = "/"
    | t == "\\\\" = "\\"
    | t == "\\\"" = "\""
    | t == "\\$"  = "$"
    | Text.isPrefixOf "\\u{" t =
        case decodeUnicode (Text.drop 3 (Text.dropEnd 1 t)) of
            Just c  -> Text.singleton c
            Nothing -> t
    | Text.isPrefixOf "\\u" t =
        case decodeUnicode (Text.drop 2 t) of
            Just c  -> Text.singleton c
            Nothing -> t
    | otherwise = t

tToMap :: [(Text, v)] -> TParser (Dhall.Map.Map Text v)
tToMap kvs = Dhall.Map.unorderedTraverseWithKey (\_k v -> v) m
  where
    m = Dhall.Map.fromListWithKey err (map (\(k, v) -> (k, pure v)) kvs)
    err k _v1 _v2 = fail ("duplicate field: " ++ Text.unpack k)

tToMapWith
    :: (Text -> TParser v -> TParser v -> TParser v)
    -> [(Text, v)]
    -> TParser (Dhall.Map.Map Text v)
tToMapWith combine kvs = sequence m
  where
    m = Dhall.Map.fromListWithKey combine (map (\(k, v) -> (k, pure v)) kvs)

unescapeSingle :: Text -> Text
unescapeSingle t
    | t == "'''"  = "''"
    | t == "''${" = "${"
    | otherwise   = t

decodeUnicode :: Text -> Maybe Char
decodeUnicode hex =
    case reads ("0x" <> Text.unpack hex) of
        [(n, "")] | n <= 0x10FFFD && CharToken.validCodepoint n ->
            Just (Char.chr n)
        _ -> Nothing

data ApplicationExprInfo
    = NakedMergeOrSomeOrToMap
    | ImportExpr
    | ApplicationExpr
