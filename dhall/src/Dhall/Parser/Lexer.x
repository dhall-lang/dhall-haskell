{
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Dhall.Parser.Lexer
    ( Alex(..)
    , AlexPosn(..)
    , runAlex
    , alexMonadScan
    , alexError
    , alexGetInput
    , alexSetInput
    , AlexUserState(..)
    , alexInitUserState
    , alexGetUserState
    , alexSetUserState
    ) where

import Data.Bits       ((.&.))
import Data.Char       (GeneralCategory (..), chr, digitToInt, generalCategory, isHexDigit)
import Data.List       (foldl')
import Data.Text       (Text)
import Numeric         (readHex)
import Numeric.Natural (Natural)
import Text.Megaparsec (SourcePos(..), mkPos)

import Dhall.Parser.Combinators (Parser(..), unParser)
import Dhall.Parser.TokenType
import Dhall.Syntax             (CharacterSet(..))

import qualified Data.ByteString.Base16   as Base16
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text.Encoding
import qualified Dhall.Parser.Expression as Expression
import qualified Dhall.Parser.Token      as Token
import qualified Text.Megaparsec
}

%wrapper "monadUserState-strict-text"
%encoding "utf8"

$digit    = [0-9]
$hexdig   = [0-9A-Fa-f]
$bindig   = [01]
$ident0   = [A-Za-z_]
$ident1   = [A-Za-z0-9_\x2D\x2F]
$dqplain  = [^\x22\x24\x5C]
$sqplain  = [^\x27\x24]
@ident    = $ident0 $ident1*
@natural  = [1-9] $digit* | 0
@integer  = [\+\-] @natural
@exp      = [eE] [\+\-]? $digit+
@double   = [\+\-]? $digit+ ("." $digit+ @exp? | @exp)
@bytes    = "0x" \" ($hexdig $hexdig)* \"
@date     = $digit $digit $digit $digit "-" $digit $digit "-" $digit $digit
@time     = $digit $digit ":" $digit $digit ":" $digit $digit ("." $digit+)?
@tzh      = [\+\-] $digit $digit ":" $digit $digit

tokens :-

<0>  $white                               { whiteRun }
<0>  \r\n                               { trivia }
<0>  "--" [^\n]* \n                     { trivia }
<0>  "--" [^\n]* \r\n                   { trivia }
<0>  "--" [^\n]*                        { trivia }
<0>  "{-"                               { beginComment }

<0>  "#!" [^\n]* \n                     { shebang }
<0>  "#!" [^\n]* \r\n                   { shebang }

<0>  "if"                               { keyword TkIf }
<0>  "then"                             { keyword TkThen }
<0>  "else"                             { keyword TkElse }
<0>  "let"                              { keyword TkLet }
<0>  "in"                               { keyword TkIn }
<0>  "as"                               { keyword TkAs }
<0>  "using"                            { keyword TkUsing }
<0>  "merge"                            { keyword TkMerge }
<0>  "toMap"                            { keyword TkToMap }
<0>  "showConstructor"                  { keyword TkShowConstructor }
<0>  "assert"                           { keyword TkAssert }
<0>  "with"                             { keyword TkWith }
<0>  "Some"                             { keyword TkSome }
<0>  "missing"                          { keyword TkMissing }
<0>  "forall"                           { keyword (TkForall ASCII) }
<0>  ∀                                  { keyword (TkForall Unicode) }
<0>  "Infinity"                         { emitKind (TkInfinity False) }
<0>  "+Infinity"                        { emitKind (TkInfinity False) }
<0>  "-Infinity"                        { emitKind (TkInfinity True) }
<0>  "NaN"                              { emitKind TkNaN }

<0>  "||"                               { emitKind TkOr }
<0>  "&&"                               { emitKind TkAnd }
<0>  "++"                               { emitKind TkTextAppend }
<0>  "//"                               { slashSlashTok }
<0>  "/"                                { slashTok }
<0>  "==="                              { emitKind (TkEquiv ASCII) }
<0>  "=="                               { emitKind TkEQ }
<0>  "!="                               { emitKind TkNE }
<0>  "->"                               { emitKind (TkArrow ASCII) }
<0>  "::"                               { emitKind TkDoubleColon }
<0>  ∧                                  { emitKind (TkCombine Unicode) }
<0>  ⩓                                  { emitKind (TkCombineTypes Unicode) }
<0>  ⫽                                  { emitKind (TkPrefer Unicode) }
<0>  ≡                                  { emitKind (TkEquiv Unicode) }
<0>  →                                  { emitKind (TkArrow Unicode) }
<0>  λ                                  { emitKind (TkLambda Unicode) }
<0>  \\                                 { emitKind (TkLambda ASCII) }

<0>  "+"                                { emitKind TkPlus }
<0>  "*"                                { emitKind TkTimes }
<0>  "#"                                { emitKind TkListAppend }
<0>  "?"                                { emitKind TkImportAlt }
<0>  "."                                { emitKind TkDot }
<0>  "="                                { emitKind TkEqual }
<0>  ":"                                { emitKind TkColon }
<0>  "@"                                { emitKind TkAt }
<0>  "{"                                { openBrace }
<0>  "}"                                { closeBrace }
<0>  "["                                { emitKind TkBrackL }
<0>  "]"                                { emitKind TkBrackR }
<0>  "<"                                { emitKind TkAngleL }
<0>  ">"                                { emitKind TkAngleR }
<0>  "("                                { emitKind TkParenL }
<0>  ")"                                { emitKind TkParenR }
<0>  "|"                                { emitKind TkBar }
<0>  ","                                { emitKind TkComma }

<0>  ` [^`]* `                          { quotedLabel }
<0>  \"                                 { beginDq }
<0>  "''" \r\n                          { beginSq }
<0>  "''" \n                            { beginSq }

<0>  "env:"                             { extendEnv }
<0>  "http://"                          { extendHttp }
<0>  "https://"                         { extendHttp }
<0>  "sha256:"                          { extendHash }
<0>  "../"                              { extendPath }
<0>  "./"                               { extendPath }
<0>  "~/"                               { extendPath }

<0>  @date T @time (@tzh | Z | z)?      { temporalTok }
<0>  @date t @time (@tzh | Z | z)?      { temporalTok }
<0>  @time @tzh                         { temporalTok }
<0>  @date                              { temporalTok }
<0>  @time                              { temporalTok }
<0>  @time (Z | z)                       { temporalTok }
<0>  @tzh                               { temporalTok }

<0>  @bytes                             { bytesTok }
<0>  @double                            { doubleTok }
<0>  [\+\-] "0x" $hexdig                 { signedHexNaturalTok }
<0>  [\+\-] "0b" $bindig                 { signedBinNaturalTok }
<0>  @integer                           { integerTok }
<0>  "0x" $hexdig                       { hexNaturalTok }
<0>  "0b" $bindig                       { binNaturalTok }
<0>  0 [0-9] $digit*                    { leadingZeroTok }
<0>  @natural                           { naturalTok }
<0>  @ident                             { identTok }

<strDq> \"                              { endDq }
<strDq> "${"                            { interpOpen }
<strDq> \\                              { dqEscape }
<strDq> $dqplain                         { strChunk }
<strDq> \$                              { strChunk }
<strDq> \n                              { strChunk }
<strDq> \r                              { strChunk }

<strSq> "'''"                           { strChunk }
<strSq> "''${"                          { strChunk }
<strSq> "${"                            { interpOpen }
<strSq> "''"                            { endSq }
<strSq> $sqplain                         { strChunk }
<strSq> \$                              { strChunk }
<strSq> .                               { strChunk }
<strSq> \n                              { strChunk }
<strSq> \r                              { strChunk }

{
data AlexUserState = AlexUserState
    { usFile        :: String
    , usInterpStack :: [(Int, Int)]  -- (start code, brace depth)
    , usBraceDepth  :: Int
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { usFile        = ""
    , usInterpStack = []
    , usBraceDepth  = 0
    }

alexEOF :: Alex Tok
alexEOF = do
    inp <- alexGetInput
    let (pos, _, _, _) = inp
    sc <- alexGetStartCode
    case sc of
        _ | sc == strDq   -> alexError "unterminated double-quoted string"
        _ | sc == strSq   -> alexError "unterminated single-quoted string"
        _                 -> mkTokM pos pos "" TkEOF

alexAddr :: AlexPosn -> Int
alexAddr (AlexPn a _ _) = a

posnToSourcePos :: String -> AlexPosn -> SourcePos
posnToSourcePos file (AlexPn _ line col) =
    SourcePos file (mkPos line) (mkPos col)

movePos :: AlexPosn -> Char -> AlexPosn
movePos (AlexPn a l _) '\n' = AlexPn (a + 1) (l + 1) 1
movePos (AlexPn a l c) '\t' =
    AlexPn (a + 1) l (c + 8 - ((c - 1) `mod` 8))
movePos (AlexPn a l c) _    = AlexPn (a + 1) l (c + 1)

advancePosText :: AlexPosn -> Text -> AlexPosn
advancePosText p t = Text.foldl' movePos p t

advancePosAscii :: AlexPosn -> Int -> AlexPosn
advancePosAscii (AlexPn a l c) n = AlexPn (a + n) l (c + n)

matched :: AlexInput -> Int -> (AlexPosn, Text)
matched (pos, _, _, str) len = (pos, Text.take len str)

mkTokM :: AlexPosn -> AlexPosn -> Text -> TokKind -> Alex Tok
mkTokM start end txt kind = mkTokText start end txt kind

mkTokText :: AlexPosn -> AlexPosn -> Text -> TokKind -> Alex Tok
mkTokText start end txt kind = do
    file <- usFile <$> alexGetUserState
    return
        ( Tok
            { tokOff    = alexAddr start
            , tokEndOff = alexAddr end
            , tokStart  = posnToSourcePos file start
            , tokEnd    = posnToSourcePos file end
            , tokText   = txt
            , tokKind   = kind
            }
        )

emitKind :: TokKind -> AlexInput -> Int -> Alex Tok
emitKind kind inp len = do
    after <- alexGetInput
    let (pos, txt) = matched inp len
        (end, _, _, _) = after
    mkTokM pos end txt kind

keyword :: TokKind -> AlexInput -> Int -> Alex Tok
keyword = emitKind

trivia :: AlexInput -> Int -> Alex Tok
trivia inp len = emitKind TkTrivia inp len

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

whiteRun :: AlexInput -> Int -> Alex Tok
whiteRun inp len = do
    after <- alexGetInput
    let (start, _) = matched inp len
        (_, _, _, rest) = after
        extra           = Text.takeWhile isWhiteSpace rest
        n               = len + Text.length extra
        (_, _, _, str)  = inp
        full            = Text.take n str
        end             =
            if Text.all (\c -> c == ' ') full
                then advancePosAscii start n
                else advancePosText start full
        leftover        = Text.drop n str
    alexSetInput (end, '\n', [], leftover)
    mkTokM start end full TkTrivia

extendRun :: AlexInput -> Int -> (Char -> Bool) -> TokKind -> Alex Tok
extendRun inp len isPlain kind = do
    after <- alexGetInput
    let (start, prefix) = matched inp len
        (_, _, _, rest) = after
        (extra, leftover) =
            if Text.all isPlain prefix
                then Text.span isPlain rest
                else ("", rest)
        full = prefix <> extra
        end  = advancePosText start full
    alexSetInput (end, '\n', [], leftover)
    mkTokM start end full kind

shebang :: AlexInput -> Int -> Alex Tok
shebang inp len = do
    let (pos@(AlexPn _ _ col), _) = matched inp len
    if col == 1
        then emitKind TkShebang inp len
        else alexError "unexpected shebang"

quotedLabel :: AlexInput -> Int -> Alex Tok
quotedLabel inp len = emitKind TkQuotedLabel inp len

identTok :: AlexInput -> Int -> Alex Tok
identTok inp len = do
    let txt = snd (matched inp len)
    emitKind (classifyIdent txt) inp len

classifyIdent :: Text -> TokKind
classifyIdent txt
    | isBuiltin txt = TkBuiltin
    | otherwise     = TkIdent

isBuiltin :: Text -> Bool
isBuiltin txt =
    txt `elem`
        [ "Natural/fold","Natural/build","Natural/isZero","Natural/even"
        , "Natural/odd","Natural/toInteger","Natural/show","Natural/subtract"
        , "Integer","Integer/clamp","Integer/negate","Integer/show","Integer/toDouble"
        , "Double/show","List/build","List/fold","List/length","List/head"
        , "List/last","List/indexed","List/reverse","Text/show","Text/replace"
        , "Date/show","Time/show","TimeZone/show","Bool","True","False"
        , "Optional","None","Natural","Double","Text","Bytes","Date","Time"
        , "TimeZone","List","Type","Kind","Sort"
        ]

leadingZeroTok :: AlexInput -> Int -> Alex Tok
leadingZeroTok _ _ = alexError "Natural literals cannot have leading zeros"

naturalTok :: AlexInput -> Int -> Alex Tok
naturalTok inp len = do
    after <- alexGetInput
    let (pos, txt) = matched inp len
        (end, _, _, _) = after
    case parseNaturalText txt of
        Nothing -> alexError ("invalid natural: " ++ Text.unpack txt)
        Just n  -> mkTokM pos end txt (TkNatural n)

hexNaturalTok :: AlexInput -> Int -> Alex Tok
hexNaturalTok inp len = prefixedNatural inp len isHexDigit

binNaturalTok :: AlexInput -> Int -> Alex Tok
binNaturalTok inp len = prefixedNatural inp len isBinDigit

signedHexNaturalTok :: AlexInput -> Int -> Alex Tok
signedHexNaturalTok inp len = signedPrefixedNatural inp len isHexDigit

signedBinNaturalTok :: AlexInput -> Int -> Alex Tok
signedBinNaturalTok inp len = signedPrefixedNatural inp len isBinDigit

isBinDigit :: Char -> Bool
isBinDigit c = c == '0' || c == '1'

signedPrefixedNatural :: AlexInput -> Int -> (Char -> Bool) -> Alex Tok
signedPrefixedNatural inp len isDigit = do
    after <- alexGetInput
    let (start, _) = matched inp len
        (_, _, _, rest) = after
        (more, leftover) = Text.span isDigit rest
        n                = len + Text.length more
        (_, _, _, str)   = inp
        full             = Text.take n str
        end              = advancePosAscii start n
    alexSetInput (end, '\n', [], leftover)
    let (sign, unsigned) = case Text.uncons full of
            Just ('-', xs) -> (-1, xs)
            Just ('+', xs) -> (1, xs)
            _              -> (1, full)
    case parseNaturalText unsigned of
        Nothing -> alexError ("invalid integer: " ++ Text.unpack full)
        Just n  -> mkTokM start end full (TkInteger (sign * fromIntegral n))

prefixedNatural :: AlexInput -> Int -> (Char -> Bool) -> Alex Tok
prefixedNatural inp len isDigit = do
    after <- alexGetInput
    let (start, _) = matched inp len
        (_, _, _, rest) = after
        (more, leftover) = Text.span isDigit rest
        n                = len + Text.length more
        (_, _, _, str)   = inp
        full             = Text.take n str
        end              = advancePosAscii start n
    alexSetInput (end, '\n', [], leftover)
    case parseNaturalText full of
        Nothing -> alexError ("invalid natural: " ++ Text.unpack full)
        Just n  -> mkTokM start end full (TkNatural n)

integerTok :: AlexInput -> Int -> Alex Tok
integerTok inp len = do
    after <- alexGetInput
    let (pos, txt) = matched inp len
        (end, _, _, _) = after
        (sign, rest) = case Text.uncons txt of
            Just ('+', xs) -> (1, xs)
            Just ('-', xs) -> (-1, xs)
            _              -> (1, txt)
    case parseNaturalText rest of
        Nothing -> alexError ("invalid integer: " ++ Text.unpack txt)
        Just n  -> mkTokM pos end txt (TkInteger (sign * fromIntegral n))

doubleTok :: AlexInput -> Int -> Alex Tok
doubleTok inp len = do
    let txt = snd (matched inp len)
    case reads (Text.unpack txt) of
        [(d, "")] -> emitKind (TkDouble d) inp len
        _         -> alexError ("invalid double: " ++ Text.unpack txt)

bytesTok :: AlexInput -> Int -> Alex Tok
bytesTok inp len = do
    let txt = snd (matched inp len)
        hex = Text.take (max 0 (Text.length txt - 4)) (Text.drop 3 txt)
    case Base16.decode (Text.Encoding.encodeUtf8 hex) of
        Left err -> alexError err
        Right bs -> emitKind (TkBytes bs) inp len

temporalTok :: AlexInput -> Int -> Alex Tok
temporalTok inp len = emitKind TkTemporal inp len

parseNaturalText :: Text -> Maybe Natural
parseNaturalText t =
    case Text.uncons t of
        Just ('0', rest) ->
            case Text.uncons rest of
                Just ('b', digits) ->
                    if Text.null digits
                        then Nothing
                        else Just (foldDigits 2 digits)
                Just ('x', digits) ->
                    if Text.null digits
                        then Nothing
                        else Just (foldDigits 16 digits)
                _ ->
                    if Text.null rest
                        then Just 0
                        else Nothing
        Just (d, rest) ->
            if d >= '1' && d <= '9'
                then Just (foldDigits 10 (Text.cons d rest))
                else Nothing
        _ -> Nothing

foldDigits :: Int -> Text -> Natural
foldDigits base digits =
    fromIntegral
        (Text.foldl'
            (\n c -> n * toInteger base + toInteger (digitToInt c))
            0
            digits)

parseMatch :: Parser a -> Text -> Either String Text
parseMatch p str =
    case Text.Megaparsec.parse (unParser (Text.Megaparsec.match p)) "" str of
        Left bundle -> Left (Text.Megaparsec.errorBundlePretty bundle)
        Right (consumed, _) -> Right consumed

skipExtra :: Int -> Alex ()
skipExtra n
    | n <= 0 = return ()
    | otherwise = do
        inp <- alexGetInput
        let (pos, c, bs, str) = inp
            (taken, rest) = Text.splitAt n str
            pos' = advancePosText pos taken
        alexSetInput (pos', c, bs, rest)

emitLexeme :: Int -> AlexInput -> Int -> TokKind -> Alex Tok
emitLexeme n inp len kind = do
    let (pos, _, _, str) = inp
        txt = Text.take n str
        end = advancePosText pos txt
    skipExtra (n - len)
    mkTokM pos end txt kind

slashSlashTok :: AlexInput -> Int -> Alex Tok
slashSlashTok inp len =
    let (_, _, _, str) = inp
    in if Text.isPrefixOf "//\\\\" str
        then emitLexeme 4 inp len (TkCombineTypes ASCII)
        else emitLexeme 2 inp len (TkPrefer ASCII)

slashTok :: AlexInput -> Int -> Alex Tok
slashTok inp len =
    let (_, _, _, str) = inp
    in if Text.isPrefixOf "/\\" str
        then emitLexeme 2 inp len (TkCombine ASCII)
        else extendPath inp len

dqEscape :: AlexInput -> Int -> Alex Tok
dqEscape inp _len = do
    after <- alexGetInput
    let (pos, _, _, _) = inp
        (_, _, _, rest) = after
        finish n = do
            let full = Text.cons '\\' (Text.take (n - 1) rest)
                end  = advancePosText pos full
            skipExtra (n - 1)
            mkTokM pos end full TkStringChunk
    case Text.uncons rest of
        Just ('"', _)  -> finish 2
        Just ('$', _)  -> finish 2
        Just ('/', _)  -> finish 2
        Just ('\\', _) -> finish 2
        Just ('b', _)  -> finish 2
        Just ('f', _)  -> finish 2
        Just ('n', _)  -> finish 2
        Just ('r', _)  -> finish 2
        Just ('t', _)  -> finish 2
        Just ('u', urest) ->
            case Text.uncons urest of
                Just ('{', hexrest) ->
                    let (hex, afterHex) = Text.span isHexDigit hexrest
                    in case Text.uncons afterHex of
                        Just ('}', _)
                            | not (Text.null hex)
                            , Just n <- readHexInt (Text.unpack hex)
                            , validCp True n ->
                                finish (4 + Text.length hex)
                        _ -> alexError "Invalid escape sequence"
                _ ->
                    case Text.unpack (Text.take 4 urest) of
                        [a, b, c, d]
                            | all isHexDigit [a, b, c, d]
                            , Just n <- readHexInt [a, b, c, d]
                            , validCp False n -> finish 6
                        _ -> alexError "Invalid escape sequence"
        _ -> alexError "Invalid escape sequence"

readHexInt :: String -> Maybe Int
readHexInt s =
    case readHex s of
        [(n, "")] -> Just n
        _         -> Nothing

validCp :: Bool -> Int -> Bool
validCp braced n
    | n < 0 || n > 0x10FFFF = False
    | braced && n > 0x10FFFD = False
    | otherwise =
        not (category == Surrogate
          || n .&. 0xFFFE == 0xFFFE
          || n .&. 0xFFFF == 0xFFFF)
  where
    category = generalCategory (chr n)

extendWith :: Parser a -> TokKind -> AlexInput -> Int -> Alex Tok
extendWith p kind inp len = do
    let (pos, _, _, str) = inp
    case parseMatch p str of
        Left err -> alexError err
        Right consumed -> do
            skipExtra (Text.length consumed - len)
            let end = advancePosText pos consumed
            mkTokM pos end consumed kind

extendPath :: AlexInput -> Int -> Alex Tok
extendPath = extendWith Expression.localOnly TkPath

extendHttp :: AlexInput -> Int -> Alex Tok
extendHttp = extendWith Token.httpRaw TkHttpRaw

extendEnv :: AlexInput -> Int -> Alex Tok
extendEnv = extendWith Expression.env TkEnv

extendHash :: AlexInput -> Int -> Alex Tok
extendHash = extendWith Expression.importHash_ TkHash

takeBlockComment :: Text -> Maybe Int
takeBlockComment = go 0 1
  where
    go !n !d t
        | d == 0      = Just n
        | Text.null t = Nothing
        | Text.isPrefixOf "{-" t =
            go (n + 2) (d + 1) (Text.drop 2 t)
        | Text.isPrefixOf "-}" t =
            go (n + 2) (d - 1) (Text.drop 2 t)
        | otherwise =
            go (n + 1) d (Text.drop 1 t)

beginComment :: AlexInput -> Int -> Alex Tok
beginComment inp len = do
    let (start, open) = matched inp len
    after <- alexGetInput
    let (_, _, _, rest) = after
    case takeBlockComment rest of
        Nothing -> alexError "unterminated block comment"
        Just n  -> do
            let body = Text.take n rest
                full = open <> body
                end  = advancePosText start full
                leftover = Text.drop n rest
            alexSetInput (end, '\n', [], leftover)
            mkTokM start end full TkTrivia

beginDq :: AlexInput -> Int -> Alex Tok
beginDq inp len = do
    alexSetStartCode strDq
    emitKind TkDQuote inp len

endDq :: AlexInput -> Int -> Alex Tok
endDq inp len = do
    alexSetStartCode 0
    emitKind TkDQuote inp len

beginSq :: AlexInput -> Int -> Alex Tok
beginSq inp len = do
    alexSetStartCode strSq
    emitKind TkSQuoteBegin inp len

endSq :: AlexInput -> Int -> Alex Tok
endSq inp len = do
    alexSetStartCode 0
    emitKind TkSQuoteEnd inp len

isDqPlain :: Char -> Bool
isDqPlain c = c /= '"' && c /= '\x24' && c /= '\\'

isSqPlain :: Char -> Bool
isSqPlain c = c /= '\'' && c /= '\x24'

strChunk :: AlexInput -> Int -> Alex Tok
strChunk inp len = do
    sc <- alexGetStartCode
    let isPlain = if sc == strSq then isSqPlain else isDqPlain
    extendRun inp len isPlain TkStringChunk

interpOpen :: AlexInput -> Int -> Alex Tok
interpOpen inp len = do
    sc <- alexGetStartCode
    ust <- alexGetUserState
    alexSetUserState ust
        { usInterpStack = (sc, usBraceDepth ust) : usInterpStack ust
        , usBraceDepth  = 0
        }
    alexSetStartCode 0
    emitKind TkInterpOpen inp len

openBrace :: AlexInput -> Int -> Alex Tok
openBrace inp len = do
    ust <- alexGetUserState
    alexSetUserState ust { usBraceDepth = usBraceDepth ust + 1 }
    emitKind TkBraceL inp len

closeBrace :: AlexInput -> Int -> Alex Tok
closeBrace inp len = do
    ust <- alexGetUserState
    case usInterpStack ust of
        ((prev, prevDepth):rest)
            | usBraceDepth ust == 0 -> do
                alexSetUserState ust
                    { usInterpStack = rest
                    , usBraceDepth  = prevDepth
                    }
                alexSetStartCode prev
                emitKind TkInterpClose inp len
        _ -> do
            let depth = max 0 (usBraceDepth ust - 1)
            alexSetUserState ust { usBraceDepth = depth }
            emitKind TkBraceR inp len
}
