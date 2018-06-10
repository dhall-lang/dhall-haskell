{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | This module contains Dhall's parsing logic

module Dhall.Parser (
    -- * Utilities
      exprFromText
    , exprAndHeaderFromText

    -- * Parsers
    , expr, exprA

    -- * Types
    , Src(..)
    , ParseError(..)
    , Parser(..)
    , module Dhall.Parser.Combinators
    ) where

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
import Dhall.Parser.Expression

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

scheme :: Parser Text
scheme = "http" <> option "s"

httpRaw :: Parser (Text, File, Text)
httpRaw = do
    prefixText <- scheme <> "://" <> authority
    file   <- file_
    suffixText <- option ("?" <> query) <> option ("#" <> fragment)

    return (prefixText, file, suffixText)

authority :: Parser Text
authority = option (try (userinfo <> "@")) <> host <> option (":" <> port)

userinfo :: Parser Text
userinfo = star (satisfy predicate <|> pctEncoded)
  where
    predicate c = unreserved c || subDelims c || c == ':'

host :: Parser Text
host = choice [ ipLiteral, ipV4Address, regName ]

port :: Parser Text
port = star (satisfy digit)

ipLiteral :: Parser Text
ipLiteral = "[" <> (ipV6Address <|> ipVFuture) <> "]"

ipVFuture :: Parser Text
ipVFuture = "v" <> plus (satisfy hexdig) <> "." <> plus (satisfy predicate)
  where
    predicate c = unreserved c || subDelims c || c == ':'

ipV6Address :: Parser Text
ipV6Address =
    choice
        [ try alternative0
        , try alternative1
        , try alternative2
        , try alternative3
        , try alternative4
        , try alternative5
        , try alternative6
        , try alternative7
        ,     alternative8
        ]
  where
    alternative0 = count 6 (h16 <> ":") <> ls32

    alternative1 = "::" <> count 5 (h16 <> ":") <> ls32

    alternative2 = option h16 <> "::" <> count 4 (h16 <> ":") <> ls32

    alternative3 =
            option (range 0 1 (h16 <> ":") <> h16)
        <>  "::"
        <>  count 3 (h16 <> ":")
        <>  ls32

    alternative4 =
            option (range 0 2 (h16 <> ":") <> h16)
        <>  "::"
        <>  count 2 (h16 <> ":")
        <>  ls32

    alternative5 =
        option (range 0 3 (h16 <> ":") <> h16) <> "::" <> h16 <> ":" <> ls32

    alternative6 =
        option (range 0 4 (h16 <> ":") <> h16) <> "::" <> ls32

    alternative7 =
        option (range 0 5 (h16 <> ":") <> h16) <> "::" <> h16

    alternative8 =
        option (range 0 6 (h16 <> ":") <> h16) <> "::"

h16 :: Parser Text
h16 = range 1 3 (satisfy hexdig)

ls32 :: Parser Text
ls32 = (h16 <> ":" <> h16) <|> ipV4Address

ipV4Address :: Parser Text
ipV4Address = decOctet <> "." <> decOctet <> "." <> decOctet <> "." <> decOctet

decOctet :: Parser Text
decOctet =
    choice
        [ try alternative4
        , try alternative3
        , try alternative2
        , try alternative1
        ,     alternative0
        ]
  where
    alternative0 = satisfy digit

    alternative1 = satisfy predicate <> satisfy digit
      where
        predicate c = '\x31' <= c && c <= '\x39'

    alternative2 = "1" <> count 2 (satisfy digit)

    alternative3 = "2" <> satisfy predicate <> satisfy digit
      where
        predicate c = '\x30' <= c && c <= '\x34'

    alternative4 = "25" <> satisfy predicate
      where
        predicate c = '\x30' <= c && c <= '\x35'

regName :: Parser Text
regName = star (satisfy predicate <|> pctEncoded)
  where
    predicate c = unreserved c || subDelims c

pchar :: Parser Text
pchar = satisfy predicate <|> pctEncoded
  where
    predicate c = unreserved c || subDelims c || c == ':' || c == '@'

query :: Parser Text
query = star (pchar <|> satisfy predicate)
  where
    predicate c = c == '/' || c == '?'

fragment :: Parser Text
fragment = star (pchar <|> satisfy predicate)
  where
    predicate c = c == '/' || c == '?'

pctEncoded :: Parser Text
pctEncoded = "%" <> count 2 (satisfy hexdig)

unreserved :: Char -> Bool
unreserved c =
    alpha c || digit c || c == '-' || c == '.' || c == '_' || c == '~'

subDelims :: Char -> Bool
subDelims c = c `elem` ("!$&'()*+,;=" :: String)

http :: Parser ImportType
http = do
    (prefix, path, suffix) <- httpRaw
    whitespace
    headers <- optional (do
        _using
        importHashed_ )
    return (URL prefix path suffix headers)

-- | Parser for a top-level Dhall expression
expr :: Parser (Expr Src Import)
expr = exprA import_

-- | Parser for a top-level Dhall expression. The expression is parameterized
-- over any parseable type, allowing the language to be extended as needed.
exprA :: Parser a -> Parser (Expr Src a)
exprA = completeExpression

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

-- | A parsing error
data ParseError = ParseError
    { unwrap :: Text.Megaparsec.ParseError Char Void
    , input  :: Text
    }

instance Show ParseError where
    show (ParseError {..}) =
      "\n\ESC[1;31mError\ESC[0m: Invalid input\n\n" <> Text.Megaparsec.parseErrorPretty' input unwrap

instance Exception ParseError

-- | Parse an expression from `Text` containing a Dhall program
exprFromText
  :: String -- ^ User-friendly name describing the input expression,
            --   used in parsing error messages
  -> Text   -- ^ Input expression to parse
  -> Either ParseError (Expr Src Import)
exprFromText delta text = fmap snd (exprAndHeaderFromText delta text)

{-| Like `exprFromText` but also returns the leading comments and whitespace
    (i.e. header) up to the last newline before the code begins

    In other words, if you have a Dhall file of the form:

> -- Comment 1
> {- Comment -} 2

    Then this will preserve @Comment 1@, but not @Comment 2@

    This is used by @dhall-format@ to preserve leading comments and whitespace
-}
exprAndHeaderFromText
    :: String -- ^ User-friendly name describing the input expression,
              --   used in parsing error messages
    -> Text   -- ^ Input expression to parse
    -> Either ParseError (Text, Expr Src Import)
exprAndHeaderFromText delta text = case result of
    Left errInfo   -> Left (ParseError { unwrap = errInfo, input = text })
    Right (txt, r) -> Right (Data.Text.dropWhileEnd (/= '\n') txt, r)
  where
    parser = do
        (bytes, _) <- Text.Megaparsec.match whitespace
        r <- expr
        Text.Megaparsec.eof
        return (bytes, r)

    result = Text.Megaparsec.parse (unParser parser) delta text
