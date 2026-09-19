{
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
import Data.Char       (GeneralCategory (..), chr, digitToInt, generalCategory, isDigit, isHexDigit)
import Data.Text       (Text)
import Numeric         (readHex)
import Numeric.Natural (Natural)
import Text.Megaparsec (SourcePos(..), mkPos)

import Dhall.Parser.Combinators (Parser(..), unParser)
import Dhall.Parser.TokenType
import Dhall.Syntax             (CharacterSet(..))

import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text.Encoding
import qualified Dhall.Parser.Expression as Expression
import qualified Dhall.Parser.Token      as Token
import qualified Text.Megaparsec
}

%wrapper "monadUserState"
%encoding "utf8"

$digit    = [0-9]
$hexdig   = [0-9A-Fa-f]
$bindig   = [01]
$ident0   = [A-Za-z_]

@ident    = $ident0 ([A-Za-z0-9_] | "/" | "-")*
@natural  = "0b" $bindig+ | "0x" $hexdig+ | [1-9] $digit* | 0
@integer  = [\+\-] @natural
@exp      = [eE] [\+\-]? $digit+
@double   = [\+\-]? $digit+ ("." $digit+ @exp? | @exp)
@bytes    = "0x" \" ($hexdig $hexdig)* \"
@date     = $digit $digit $digit $digit "-" $digit $digit "-" $digit $digit
@time     = $digit $digit ":" $digit $digit ":" $digit $digit ("." $digit+)?
@tzh      = [\+\-] $digit $digit ":" $digit $digit

tokens :-

<0>  $white+                          { trivia }
<0>  \r\n                             { trivia }
<0>  "--" [^\n]* \n                   { trivia }
<0>  "--" [^\n]* \r\n                 { trivia }
<0>  "--" [^\n]*                      { trivia }
<0>  "{-"                             { beginComment }

<0>  "#!" [^\n]* \n                   { shebang }
<0>  "#!" [^\n]* \r\n                 { shebang }

<0>  "if"                             { keyword TkIf }
<0>  "then"                           { keyword TkThen }
<0>  "else"                           { keyword TkElse }
<0>  "let"                            { keyword TkLet }
<0>  "in"                             { keyword TkIn }
<0>  "as"                             { keyword TkAs }
<0>  "using"                          { keyword TkUsing }
<0>  "merge"                          { keyword TkMerge }
<0>  "toMap"                          { keyword TkToMap }
<0>  "showConstructor"                { keyword TkShowConstructor }
<0>  "assert"                         { keyword TkAssert }
<0>  "with"                           { keyword TkWith }
<0>  "Some"                           { keyword TkSome }
<0>  "missing"                        { keyword TkMissing }
<0>  "forall"                         { keyword (TkForall ASCII) }
<0>  ∀                                { keyword (TkForall Unicode) }
<0>  "Infinity"                       { emitKind (TkInfinity False) }
<0>  "+Infinity"                      { emitKind (TkInfinity False) }
<0>  "-Infinity"                      { emitKind (TkInfinity True) }
<0>  "NaN"                            { emitKind TkNaN }

<0>  "||"                             { emitKind TkOr }
<0>  "&&"                             { emitKind TkAnd }
<0>  "++"                             { emitKind TkTextAppend }
<0>  "//"                             { slashSlashTok }
<0>  "/"                              { slashTok }
<0>  "==="                            { emitKind (TkEquiv ASCII) }
<0>  "=="                             { emitKind TkEQ }
<0>  "!="                             { emitKind TkNE }
<0>  "->"                             { emitKind (TkArrow ASCII) }
<0>  "::"                             { emitKind TkDoubleColon }
<0>  ∧                                { emitKind (TkCombine Unicode) }
<0>  ⩓                                { emitKind (TkCombineTypes Unicode) }
<0>  ⫽                                { emitKind (TkPrefer Unicode) }
<0>  ≡                                { emitKind (TkEquiv Unicode) }
<0>  →                                { emitKind (TkArrow Unicode) }
<0>  λ                                { emitKind (TkLambda Unicode) }
<0>  \\                               { emitKind (TkLambda ASCII) }

<0>  "+"                              { emitKind TkPlus }
<0>  "*"                              { emitKind TkTimes }
<0>  "#"                              { emitKind TkListAppend }
<0>  "?"                              { emitKind TkImportAlt }
<0>  "."                              { emitKind TkDot }
<0>  "="                              { emitKind TkEqual }
<0>  ":"                              { emitKind TkColon }
<0>  "@"                              { emitKind TkAt }
<0>  "{"                              { openBrace }
<0>  "}"                              { closeBrace }
<0>  "["                              { emitKind TkBrackL }
<0>  "]"                              { emitKind TkBrackR }
<0>  "<"                              { emitKind TkAngleL }
<0>  ">"                              { emitKind TkAngleR }
<0>  "("                              { emitKind TkParenL }
<0>  ")"                              { emitKind TkParenR }
<0>  "|"                              { emitKind TkBar }
<0>  ","                              { emitKind TkComma }

<0>  ` [^`]* `                        { quotedLabel }
<0>  \"                               { beginDq }
<0>  "''" \r\n                        { beginSq }
<0>  "''" \n                          { beginSq }

<0>  "env:"                           { extendEnv }
<0>  "http://"                        { extendHttp }
<0>  "https://"                       { extendHttp }
<0>  "sha256:"                        { extendHash }
<0>  "../"                            { extendPath }
<0>  "./"                             { extendPath }
<0>  "~/"                             { extendPath }

<0>  @date T @time (@tzh | Z | z)?    { temporalTok }
<0>  @date t @time (@tzh | Z | z)?    { temporalTok }
<0>  @time @tzh                       { temporalTok }
<0>  @date                            { temporalTok }
<0>  @time                            { temporalTok }
<0>  @time (Z | z)                     { temporalTok }
<0>  @tzh                             { temporalTok }

<0>  @bytes                           { bytesTok }
<0>  @double                          { doubleTok }
<0>  @integer                         { integerTok }
<0>  @natural                         { naturalTok }
<0>  @ident                           { identTok }

<comment> "{-"                        { nestComment }
<comment> "-}"                        { unnestComment }
<comment> .                           { commentChar }
<comment> \n                          { commentChar }
<comment> \r                          { commentChar }

<strDq> \"                            { endDq }
<strDq> "${"                          { interpOpen }
<strDq> \\                            { dqEscape }
<strDq> .                             { strChunk }
<strDq> \n                            { strChunk }
<strDq> \r                            { strChunk }

<strSq> "'''"                         { strChunk }
<strSq> "''${"                        { strChunk }
<strSq> "${"                          { interpOpen }
<strSq> "''"                          { endSq }
<strSq> .                             { strChunk }
<strSq> \n                            { strChunk }
<strSq> \r                            { strChunk }

{
data AlexUserState = AlexUserState
    { usFile         :: String
    , usInterpStack  :: [(Int, Int)]  -- (start code, brace depth)
    , usBraceDepth   :: Int
    , usCommentDepth :: Int
    , usCommentBuf   :: String
    , usCommentPos   :: AlexPosn
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { usFile         = ""
    , usInterpStack  = []
    , usBraceDepth   = 0
    , usCommentDepth = 0
    , usCommentBuf   = ""
    , usCommentPos   = AlexPn 0 1 1
    }

alexEOF :: Alex Tok
alexEOF = do
    inp <- alexGetInput
    let (pos, _, _, _) = inp
    sc <- alexGetStartCode
    case sc of
        _ | sc == comment -> alexError "unterminated block comment"
        _ | sc == strDq   -> alexError "unterminated double-quoted string"
        _ | sc == strSq   -> alexError "unterminated single-quoted string"
        _                 -> mkTokM pos pos "" TkEOF

posnToSourcePos :: String -> AlexPosn -> SourcePos
posnToSourcePos file (AlexPn _ line col) =
    SourcePos file (mkPos line) (mkPos col)

advancePos :: AlexPosn -> String -> AlexPosn
advancePos p [] = p
advancePos (AlexPn a l _) ('\n':xs) = advancePos (AlexPn (a + 1) (l + 1) 1) xs
advancePos (AlexPn a l c) (_:xs)    = advancePos (AlexPn (a + 1) l (c + 1)) xs

mkTokM :: AlexPosn -> AlexPosn -> String -> TokKind -> Alex Tok
mkTokM start end txt kind = do
    file <- usFile <$> alexGetUserState
    return (Tok 0 (posnToSourcePos file start) (posnToSourcePos file end) (Text.pack txt) kind)

matched :: AlexInput -> Int -> (AlexPosn, String)
matched (pos, _, _, str) len = (pos, take len str)

emitKind :: TokKind -> AlexInput -> Int -> Alex Tok
emitKind kind inp len = do
    let (pos, txt) = matched inp len
        end = advancePos pos txt
    mkTokM pos end txt kind

keyword :: TokKind -> AlexInput -> Int -> Alex Tok
keyword = emitKind

trivia :: AlexInput -> Int -> Alex Tok
trivia inp len = emitKind (TkTrivia (Text.pack (snd (matched inp len)))) inp len

shebang :: AlexInput -> Int -> Alex Tok
shebang inp len = do
    let (pos@(AlexPn _ _ col), txt) = matched inp len
    if col == 1
        then emitKind (TkShebang (Text.pack txt)) inp len
        else alexError "unexpected shebang"

quotedLabel :: AlexInput -> Int -> Alex Tok
quotedLabel inp len = do
    let txt = snd (matched inp len)
        inner = drop 1 (take (length txt - 1) txt)
    emitKind (TkQuotedLabel (Text.pack inner)) inp len

identTok :: AlexInput -> Int -> Alex Tok
identTok inp len = do
    let txt = snd (matched inp len)
    emitKind (classifyIdent txt) inp len

classifyIdent :: String -> TokKind
classifyIdent s
    | isBuiltin s = TkBuiltin (Text.pack s)
    | otherwise   = TkIdent (Text.pack s)

isBuiltin :: String -> Bool
isBuiltin s =
    s `elem`
        [ "Natural/fold","Natural/build","Natural/isZero","Natural/even"
        , "Natural/odd","Natural/toInteger","Natural/show","Natural/subtract"
        , "Integer","Integer/clamp","Integer/negate","Integer/show","Integer/toDouble"
        , "Double/show","List/build","List/fold","List/length","List/head"
        , "List/last","List/indexed","List/reverse","Text/show","Text/replace"
        , "Date/show","Time/show","TimeZone/show","Bool","True","False"
        , "Optional","None","Natural","Double","Text","Bytes","Date","Time"
        , "TimeZone","List","Type","Kind","Sort"
        ]

naturalTok :: AlexInput -> Int -> Alex Tok
naturalTok inp len = do
    let txt = snd (matched inp len)
    case parseNatural txt of
        Nothing -> alexError ("invalid natural: " ++ txt)
        Just n  -> emitKind (TkNatural n) inp len

integerTok :: AlexInput -> Int -> Alex Tok
integerTok inp len = do
    let txt = snd (matched inp len)
        (sign, rest) = case txt of
            '+':xs -> (1, xs)
            '-':xs -> (-1, xs)
            xs     -> (1, xs)
    case parseNatural rest of
        Nothing -> alexError ("invalid integer: " ++ txt)
        Just n  -> emitKind (TkInteger (sign * fromIntegral n)) inp len

doubleTok :: AlexInput -> Int -> Alex Tok
doubleTok inp len = do
    let txt = snd (matched inp len)
    case reads txt of
        [(d, "")] -> emitKind (TkDouble d) inp len
        _         -> alexError ("invalid double: " ++ txt)

bytesTok :: AlexInput -> Int -> Alex Tok
bytesTok inp len = do
    let txt = snd (matched inp len)
        hex = drop 3 (take (length txt - 1) txt)
    case Base16.decode (Text.Encoding.encodeUtf8 (Text.pack hex)) of
        Left err -> alexError err
        Right bs -> emitKind (TkBytes bs) inp len

temporalTok :: AlexInput -> Int -> Alex Tok
temporalTok inp len =
    emitKind (TkTemporal (Text.pack (snd (matched inp len)))) inp len

parseNatural :: String -> Maybe Natural
parseNatural ('0':'b':rest)
    | not (null rest) && all (\c -> c == '0' || c == '1') rest =
        Just (fromIntegral (foldl (\n c -> n * 2 + digitToInt c) 0 rest))
parseNatural ('0':'x':rest)
    | not (null rest) && all isHexDigit rest =
        case readHex rest of
            [(n, "")] -> Just (fromIntegral (n :: Integer))
            _         -> Nothing
parseNatural "0" = Just 0
parseNatural s@(d:rest)
    | d >= '1' && d <= '9' && all isDigit rest =
        Just (fromIntegral (read s :: Integer))
parseNatural _ = Nothing

parseMatch :: Parser a -> String -> Either String Text
parseMatch p str =
    case Text.Megaparsec.parse (unParser (Text.Megaparsec.match p)) "" (Text.pack str) of
        Left bundle -> Left (Text.Megaparsec.errorBundlePretty bundle)
        Right (consumed, _) -> Right consumed

skipExtra :: Int -> Alex ()
skipExtra n
    | n <= 0 = return ()
    | otherwise = do
        inp <- alexGetInput
        let (pos, _, _, str) = inp
            (taken, rest) = splitAt n str
            pos' = advancePos pos taken
        alexSetInput (pos', '\n', [], rest)

emitLexeme :: Int -> AlexInput -> Int -> TokKind -> Alex Tok
emitLexeme n inp len kind = do
    let (pos, _, _, str) = inp
        txt = take n str
        end = advancePos pos txt
    skipExtra (n - len)
    mkTokM pos end txt kind

slashSlashTok :: AlexInput -> Int -> Alex Tok
slashSlashTok inp len =
    let (_, _, _, str) = inp
    in case str of
        '/':'/':'\\':'\\':_ -> emitLexeme 4 inp len (TkCombineTypes ASCII)
        _                   -> emitLexeme 2 inp len (TkPrefer ASCII)

slashTok :: AlexInput -> Int -> Alex Tok
slashTok inp len =
    let (_, _, _, str) = inp
    in case str of
        '/':'\\':_ -> emitLexeme 2 inp len (TkCombine ASCII)
        _          -> extendPath inp len

dqEscape :: AlexInput -> Int -> Alex Tok
dqEscape inp _len = do
    after <- alexGetInput
    let (pos, _, _, _) = inp
        (_, _, _, rest) = after
        finish n = do
            let full = '\\' : take (n - 1) rest
                end  = advancePos pos full
            skipExtra (n - 1)
            mkTokM pos end full (TkStringChunk (Text.pack full))
    case rest of
        '"':_  -> finish 2
        '$':_  -> finish 2
        '/':_  -> finish 2
        '\\':_ -> finish 2
        'b':_  -> finish 2
        'f':_  -> finish 2
        'n':_  -> finish 2
        'r':_  -> finish 2
        't':_  -> finish 2
        'u':'{':xs ->
            let (hex, afterHex) = span isHexDigit xs
            in case afterHex of
                '}':_ | not (null hex), Just n <- readHexInt hex, validCp True n ->
                    finish (4 + length hex)
                _ -> alexError "Invalid escape sequence"
        'u':a:b:c:d:_
            | all isHexDigit [a, b, c, d]
            , Just n <- readHexInt [a, b, c, d]
            , validCp False n -> finish 6
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

extendWith :: Parser a -> (Text -> TokKind) -> AlexInput -> Int -> Alex Tok
extendWith p mkKind inp len = do
    let (pos, _, _, str) = inp
    case parseMatch p str of
        Left err -> alexError err
        Right consumed -> do
            skipExtra (Text.length consumed - len)
            let txt = Text.unpack consumed
                end = advancePos pos txt
            mkTokM pos end txt (mkKind consumed)

extendPath :: AlexInput -> Int -> Alex Tok
extendPath = extendWith Expression.localOnly TkPath

extendHttp :: AlexInput -> Int -> Alex Tok
extendHttp = extendWith Token.httpRaw TkHttpRaw

extendEnv :: AlexInput -> Int -> Alex Tok
extendEnv = extendWith Expression.env TkEnv

extendHash :: AlexInput -> Int -> Alex Tok
extendHash = extendWith Expression.importHash_ TkHash

beginComment :: AlexInput -> Int -> Alex Tok
beginComment inp len = do
    ust <- alexGetUserState
    let (pos, txt) = matched inp len
    alexSetUserState ust
        { usCommentDepth = 1
        , usCommentBuf   = txt
        , usCommentPos   = pos
        }
    alexSetStartCode comment
    alexMonadScan

nestComment :: AlexInput -> Int -> Alex Tok
nestComment inp len = do
    ust <- alexGetUserState
    alexSetUserState ust
        { usCommentDepth = usCommentDepth ust + 1
        , usCommentBuf   = usCommentBuf ust ++ snd (matched inp len)
        }
    alexMonadScan

unnestComment :: AlexInput -> Int -> Alex Tok
unnestComment inp len = do
    ust <- alexGetUserState
    let depth = usCommentDepth ust - 1
        buf   = usCommentBuf ust ++ snd (matched inp len)
    if depth <= 0
        then do
            alexSetUserState ust { usCommentDepth = 0, usCommentBuf = "" }
            alexSetStartCode 0
            let start = usCommentPos ust
                end   = advancePos start buf
            mkTokM start end buf (TkTrivia (Text.pack buf))
        else do
            alexSetUserState ust { usCommentDepth = depth, usCommentBuf = buf }
            alexMonadScan

commentChar :: AlexInput -> Int -> Alex Tok
commentChar inp len = do
    ust <- alexGetUserState
    alexSetUserState ust { usCommentBuf = usCommentBuf ust ++ snd (matched inp len) }
    alexMonadScan

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

strChunk :: AlexInput -> Int -> Alex Tok
strChunk inp len =
    emitKind (TkStringChunk (Text.pack (snd (matched inp len)))) inp len

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
