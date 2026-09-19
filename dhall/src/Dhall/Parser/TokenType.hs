{-# LANGUAGE OverloadedStrings #-}

-- | Tokens produced by the Alex lexer.
module Dhall.Parser.TokenType
    ( Tok(..)
    , TokKind(..)
    , isTrivia
    , startsImportExpression
    ) where

import Data.ByteString (ByteString)
import Data.Text       (Text)
import Numeric.Natural (Natural)
import Text.Megaparsec (SourcePos)

import Dhall.Syntax (CharacterSet(..))

-- | A single lexeme with source span and exact source text.
data Tok = Tok
    { tokOff   :: !Int
    , tokStart :: SourcePos
    , tokEnd   :: SourcePos
    , tokText  :: Text
    , tokKind  :: TokKind
    } deriving (Eq, Ord, Show)

-- | Classification of a lexeme.
data TokKind
    = TkTrivia Text
    | TkShebang Text
    | TkIf | TkThen | TkElse | TkLet | TkIn | TkAs | TkUsing
    | TkMerge | TkToMap | TkShowConstructor | TkAssert | TkWith
    | TkSome | TkMissing
    | TkForall CharacterSet
    | TkIdent Text
    | TkQuotedLabel Text
    | TkBuiltin Text
    | TkNatural Natural
    | TkInteger Integer
    | TkDouble Double
    | TkInfinity Bool
    | TkNaN
    | TkBytes ByteString
    | TkTemporal Text
    | TkLambda CharacterSet
    | TkArrow CharacterSet
    | TkEquiv CharacterSet
    | TkCombine CharacterSet
    | TkCombineTypes CharacterSet
    | TkPrefer CharacterSet
    | TkOr | TkAnd | TkPlus | TkTimes | TkTextAppend | TkListAppend
    | TkEQ | TkNE | TkImportAlt
    | TkDot | TkDoubleColon | TkEqual | TkColon | TkAt
    | TkBraceL | TkBraceR | TkBrackL | TkBrackR | TkAngleL | TkAngleR
    | TkParenL | TkParenR | TkBar | TkComma
    | TkDQuote | TkSQuoteBegin | TkSQuoteEnd
    | TkStringChunk Text
    | TkInterpOpen
    | TkInterpClose
    | TkPath Text
    | TkEnv Text
    | TkHttpRaw Text
    | TkHash Text
    | TkEOF
    deriving (Eq, Ord, Show)

-- | Whitespace or comment tokens.
isTrivia :: TokKind -> Bool
isTrivia (TkTrivia _)  = True
isTrivia (TkShebang _) = True
isTrivia _             = False

-- | First token of an import-expression (application argument).
startsImportExpression :: TokKind -> Bool
startsImportExpression k = case k of
    TkTrivia _          -> False
    TkShebang _         -> False
    TkIf                -> False
    TkThen              -> False
    TkElse              -> False
    TkLet               -> False
    TkIn                -> False
    TkAs                -> False
    TkUsing             -> False
    TkMerge             -> False
    TkToMap             -> False
    TkShowConstructor   -> False
    TkSome              -> False
    TkWith              -> False
    TkAssert            -> False
    TkForall _          -> False
    TkArrow _           -> False
    TkEquiv _           -> False
    TkCombine _         -> False
    TkCombineTypes _    -> False
    TkPrefer _          -> False
    TkOr                -> False
    TkAnd               -> False
    TkPlus              -> False
    TkTimes             -> False
    TkTextAppend        -> False
    TkListAppend        -> False
    TkEQ                -> False
    TkNE                -> False
    TkImportAlt         -> False
    TkDot               -> False
    TkDoubleColon       -> False
    TkEqual             -> False
    TkColon             -> False
    TkAt                -> False
    TkBraceR            -> False
    TkBrackR            -> False
    TkAngleR            -> False
    TkParenR            -> False
    TkBar               -> False
    TkComma             -> False
    TkInterpClose       -> False
    TkSQuoteEnd         -> False
    TkEOF               -> False
    -- merge / Some / toMap / showConstructor / missing / literals / ids / paths
    _                   -> True
