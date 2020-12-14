{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import Control.Exception (Exception, SomeException)
import Control.DeepSeq (($!!))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Dhall.Import (Status(..))
import Dhall.Pretty (CharacterSet(..))
import Network.Wai (Application)

import Dhall.Core
    ( Directory(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , Scheme(..)
    , URL(..)
    )

import qualified Control.Exception          as Exception
import qualified Control.Monad.Except       as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.ByteString.Lazy       as ByteString
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Encoding
import qualified Dhall.Core                 as Core
import qualified Dhall.Import               as Import
import qualified Dhall.Parser               as Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck            as TypeCheck
import qualified Network.HTTP.Types         as Types
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import qualified Prettyprinter.Render.Text  as Pretty
import qualified System.Timeout             as Timeout

toBytes :: Exception e => e -> ByteString
toBytes exception = ByteString.fromStrict (Encoding.encodeUtf8 text)
  where
    text = Text.pack (show exception) <> "\n"

rootImport :: Import
rootImport =
    Import
        { importHashed = ImportHashed
            { hash = Nothing
            , importType = Remote (URL HTTPS "0.0.0.0" path_ Nothing Nothing)
            }
        , importMode = Code
        }
  where
    path_ = File (Directory []) ""

getStatus :: IO Status
getStatus = flip State.evalStateT (Import.emptyStatus ".") do
    let here = Import.chainedFromLocalHere Here (File (Directory []) "") Code

    -- This is the key bit, which sets the starting import to a URL, which
    -- turns on Dhall's various safeguards against exploits
    chainedImport <- Import.chainImport here rootImport

    return (Import.emptyStatus "."){ _stack = pure chainedImport }

application :: Status -> Application
application status request respond = do
    let die message =
            Except.throwError (Wai.responseLBS Types.status400 [] message)

    result <- (Timeout.timeout 7_000_000 . Except.runExceptT) do
        inputBytes <- liftIO (Wai.strictRequestBody request)

        inputText <- case Encoding.decodeUtf8' (ByteString.toStrict inputBytes) of
            Left  _         -> die "The request body was not valid UTF-8 text"
            Right inputText -> return inputText

        parsedExpression <- case Parser.exprFromText "(request)" inputText of
            Left exception         -> die (toBytes exception)
            Right parsedExpression -> return parsedExpression

        resolvedExpression <- do
            let load =
                    State.evalStateT (Import.loadWith parsedExpression) status

            result <- liftIO (Exception.try @SomeException load)

            case result of
                Left  exception          -> die (toBytes exception)
                Right resolvedExpression -> return resolvedExpression

        case TypeCheck.typeOf resolvedExpression of
            Left  exception -> die (toBytes exception)
            Right _         -> return ()

        let normalizedExpression = Core.normalize resolvedExpression

        let doc = Dhall.Pretty.prettyCharacterSet Unicode normalizedExpression

        let outputText = Pretty.renderStrict (Dhall.Pretty.layout doc) <> "\n"

        let outputBytes = ByteString.fromStrict (Encoding.encodeUtf8 outputText)

        evaluatedBytes <- liftIO (Exception.evaluate $!! outputBytes)

        Except.throwError (Wai.responseLBS Types.status200 [] evaluatedBytes)

    respond case result of
        Nothing ->
            Wai.responseLBS Types.status413 [] "Timeout"
        Just (Left response) ->
            response
        Just (Right response) ->
            response

main :: IO ()
main = do
    status <- getStatus

    Warp.run 3000 (application status)
