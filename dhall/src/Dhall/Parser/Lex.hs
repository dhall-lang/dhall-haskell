{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Token stream, lexer driver, and token-level combinators.
module Dhall.Parser.Lex
    ( TokenStream(..)
    , TParser
    , LexError(..)
    , lexText
    , parseTokens
    , tokensText
    , whitespace
    , nonemptyWhitespace
    , satisfyKind
    , kind_
    , peekKind
    , src
    , srcAnd
    , noted
    , runSnippet
    , convertBundle
    , lexErrorBundle
    ) where

import Control.Applicative (Alternative (..))
import Control.Monad       (void)
import Data.List.NonEmpty  (NonEmpty (..))
import Data.Text           (Text)
import Data.Void           (Void)
import Dhall.Src           (Src (..))
import Dhall.Syntax        (Expr (..))
import Text.Megaparsec     (ParseErrorBundle (..), PosState (..))

import Dhall.Parser.Combinators (Parser (..), laxSrcEq)
import Dhall.Parser.Lexer
import Dhall.Parser.TokenType

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Proxy         as Proxy
import qualified Data.Set           as Set
import qualified Data.Text          as Text
import qualified Text.Megaparsec    as Megaparsec

-- | Token stream produced by 'lexText', retaining the original source.
data TokenStream = TokenStream
    { tsSource :: !Text
    , tsTokens :: ![Tok]
    }
    deriving (Eq, Show)

-- | Token-level parser.
type TParser = Megaparsec.Parsec Void TokenStream

-- | Lexer failure.
data LexError = LexError
    { lexErrorMessage :: String
    , lexErrorInput   :: Text
    } deriving (Eq, Show)

instance Megaparsec.Stream TokenStream where
    type Token  TokenStream = Tok
    type Tokens TokenStream = [Tok]

    tokenToChunk  Proxy.Proxy = pure
    tokensToChunk Proxy.Proxy = id
    chunkToTokens Proxy.Proxy = id
    chunkLength   Proxy.Proxy = length
    chunkEmpty    Proxy.Proxy = null

    take1_ (TokenStream _ [])     = Nothing
    take1_ (TokenStream src (t:ts)) = Just (t, TokenStream src ts)

    takeN_ n s@(TokenStream src ts)
        | n <= 0    = Just ([], s)
        | null ts   = Nothing
        | otherwise =
            let (pre, post) = splitAt n ts
            in  Just (pre, TokenStream src post)

    takeWhile_ p (TokenStream src ts) =
        let (pre, post) = span p ts
        in  (pre, TokenStream src post)

instance Megaparsec.VisualStream TokenStream where
    tokensLength Proxy.Proxy = length
    showTokens   Proxy.Proxy =
        Text.unpack . Text.intercalate " " . fmap tokText . NonEmpty.toList

instance Megaparsec.TraversableStream TokenStream where
    reachOffset o pst@Megaparsec.PosState{..} =
        ( Just preview
        , pst
            { Megaparsec.pstateInput     = TokenStream src rest
            , Megaparsec.pstateOffset    = max pstateOffset o
            , Megaparsec.pstateSourcePos = pos
            }
        )
      where
        TokenStream src ts = pstateInput
        dropped            = max 0 (o - pstateOffset)
        rest               = drop dropped ts
        pos                = case rest of
            (t:_) -> tokStart t
            []    -> case reverse (take dropped ts) of
                (t:_) -> tokEnd t
                []    -> pstateSourcePos
        preview = case rest of
            (t:_) -> Text.unpack (tokText t)
            []    -> "<eof>"

    reachOffsetNoLine o pst@Megaparsec.PosState{..} =
        pst
            { Megaparsec.pstateInput     = TokenStream src rest
            , Megaparsec.pstateOffset    = max pstateOffset o
            , Megaparsec.pstateSourcePos = pos
            }
      where
        TokenStream src ts = pstateInput
        dropped            = max 0 (o - pstateOffset)
        rest               = drop dropped ts
        pos                = case rest of
            (t:_) -> tokStart t
            []    -> pstateSourcePos

-- | Slice the original source covered by a non-empty token list.
tokensText :: TokenStream -> [Tok] -> Text
tokensText _ [] = ""
tokensText (TokenStream src _) toks =
    let t0 = head toks
        t1 = last toks
    in  Text.take (tokEndOff t1 - tokOff t0) (Text.drop (tokOff t0) src)

-- | Lex @Text@ into a token stream.  Positions use @file@ as the source name.
lexText :: String -> Text -> Either LexError TokenStream
lexText file text =
    case runAlex text scanAll of
        Left err   -> Left (LexError err text)
        Right toks -> Right (TokenStream text toks)
  where
    scanAll = do
        ust <- alexGetUserState
        alexSetUserState ust { usFile = file }
        go
    go = collect []
    collect acc = do
        tok <- alexMonadScan
        case tokKind tok of
            TkEOF -> return (reverse acc)
            _     -> collect (tok : acc)

-- | Run a token parser.
parseTokens
    :: TParser a
    -> String
    -> TokenStream
    -> Either (ParseErrorBundle TokenStream Void) a
parseTokens p file stream = Megaparsec.parse p file stream

triviaTok :: TParser Tok
triviaTok = Megaparsec.satisfy (\t -> isTrivia (tokKind t)) Megaparsec.<?> "whitespace"

-- | Zero or more trivia tokens.
whitespace :: TParser ()
whitespace = void (many triviaTok)

-- | One or more trivia tokens.
nonemptyWhitespace :: TParser ()
nonemptyWhitespace = void (some triviaTok)

-- | Next token matching a kind predicate (does not skip trivia).
satisfyKind :: (TokKind -> Bool) -> TParser Tok
satisfyKind p =
    Megaparsec.satisfy (\t -> p (tokKind t))

-- | Match an exact token kind.
kind_ :: TokKind -> TParser Tok
kind_ k = satisfyKind (== k) Megaparsec.<?> show k

-- | Look at the next non-trivia token's kind.
peekKind :: TParser TokKind
peekKind = Megaparsec.lookAhead $ do
    whitespace
    tokKind <$> Megaparsec.anySingle

-- | Source span of a token parser (slice of the original input).
src :: TParser a -> TParser Src
src parser = do
    stream <- Megaparsec.getInput
    before <- Megaparsec.getSourcePos
    (toks, _) <- Megaparsec.match parser
    after <- Megaparsec.getSourcePos
    return (Src before after (tokensText stream toks))

-- | Like 'src', also returning the value.
srcAnd :: TParser a -> TParser (Src, a)
srcAnd parser = do
    stream <- Megaparsec.getInput
    before <- Megaparsec.getSourcePos
    (toks, x) <- Megaparsec.match parser
    after <- Megaparsec.getSourcePos
    return (Src before after (tokensText stream toks), x)

-- | Wrap an expression in 'Note'.
noted :: TParser (Expr Src a) -> TParser (Expr Src a)
noted parser = do
    stream <- Megaparsec.getInput
    before <- Megaparsec.getSourcePos
    (toks, e) <- Megaparsec.match parser
    after <- Megaparsec.getSourcePos
    let src0 = Src before after (tokensText stream toks)
    case e of
        Note src1 _ | laxSrcEq src0 src1 -> return e
        _                                -> return (Note src0 e)

-- | Run a character-level parser on a token's exact text.
runSnippet :: Parser a -> Text -> TParser a
runSnippet p t =
    case Megaparsec.parse (unParser (p <* Parser Megaparsec.eof)) "" t of
        Left  _ -> fail "invalid token contents"
        Right x -> return x

-- | Map a token parse error onto the original 'Text' for 'ParseError'.
convertBundle
    :: Text
    -> String
    -> TokenStream
    -> ParseErrorBundle TokenStream Void
    -> ParseErrorBundle Text Void
convertBundle original file (TokenStream _ toks) bundle =
    ParseErrorBundle
        { bundleErrors  = fmap convertError (bundleErrors bundle)
        , bundlePosState =
            PosState
                { pstateInput      = original
                , pstateOffset     = off
                , pstateSourcePos  = pos
                , pstateTabWidth   = pstateTabWidth (bundlePosState bundle)
                , pstateLinePrefix = ""
                }
        }
  where
    firstErr = NonEmpty.head (bundleErrors bundle)
    (off, pos) = locate (Megaparsec.errorOffset firstErr)

    convertError (Megaparsec.TrivialError o us es) =
        Megaparsec.TrivialError
            (fst (locate o))
            (fmap convertItem us)
            (Set.map convertItem es)
    convertError (Megaparsec.FancyError o xs) =
        Megaparsec.FancyError (fst (locate o)) xs

    convertItem (Megaparsec.Tokens ts) =
        Megaparsec.Tokens (textToChars (tokText (NonEmpty.head ts)))
    convertItem (Megaparsec.Label l) = Megaparsec.Label l
    convertItem Megaparsec.EndOfInput = Megaparsec.EndOfInput

    locate tokenOffset =
        case drop tokenOffset toks of
            (t:_) -> (tokOff t, tokStart t)
            []    -> (Text.length original, endPos)
      where
        endPos = case reverse toks of
            (t:_) -> tokEnd t
            []    -> Megaparsec.initialPos file

textToChars :: Text -> NonEmpty Char
textToChars t =
    case Text.unpack t of
        []     -> ' ' :| []
        (c:cs) -> c :| cs

-- | Convert a lexer failure into a 'Text' error bundle.
lexErrorBundle :: String -> Text -> LexError -> ParseErrorBundle Text Void
lexErrorBundle file original (LexError msg _) =
    ParseErrorBundle
        { bundleErrors =
            Megaparsec.FancyError 0 (Set.singleton (Megaparsec.ErrorFail msg)) :| []
        , bundlePosState =
            PosState
                { pstateInput      = original
                , pstateOffset     = 0
                , pstateSourcePos  = Megaparsec.initialPos file
                , pstateTabWidth   = Megaparsec.defaultTabWidth
                , pstateLinePrefix = ""
                }
        }
