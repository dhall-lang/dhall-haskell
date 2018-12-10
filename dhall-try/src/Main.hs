{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson.Encode.Pretty
import qualified Data.IORef
import qualified Data.JSString
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified GHCJS.Foreign.Callback

import Control.Exception (Exception, SomeException)
import Data.JSString (JSString)
import Data.Text (Text)
import Dhall.Core (Expr(..))
import GHCJS.Foreign.Callback (Callback)

foreign import javascript unsafe "input.getValue()" getInput :: IO JSString

foreign import javascript unsafe "input.on('change', $1)" registerInterpret :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "dhallTab.onclick = $1" registerDhallOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "jsonTab.onclick = $1" registerJSONOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "output.setValue($1)" setOutput_ :: JSString -> IO ()

foreign import javascript unsafe "selectTab($1, $2)" selectTab :: JSString -> JSString -> IO ()

fixup :: Text -> Text
fixup = Data.Text.replace "\ESC[1;31mError\ESC[0m" "Error"

setOutput :: Text -> IO ()
setOutput = setOutput_ . Data.JSString.pack . Data.Text.unpack

errOutput :: Exception e => e -> IO ()
errOutput = setOutput . fixup . Data.Text.pack . show

jsonConfig :: Data.Aeson.Encode.Pretty.Config
jsonConfig =
    Data.Aeson.Encode.Pretty.Config
        { Data.Aeson.Encode.Pretty.confIndent =
            Data.Aeson.Encode.Pretty.Spaces 2
        , Data.Aeson.Encode.Pretty.confCompare =
            compare
        , Data.Aeson.Encode.Pretty.confNumFormat =
            Data.Aeson.Encode.Pretty.Generic
        , Data.Aeson.Encode.Pretty.confTrailingNewline =
            False
        }

data Mode = Dhall | JSON deriving (Show)

main :: IO ()
main = do
    modeRef <- Data.IORef.newIORef Dhall

    let prettyExpression =
              Pretty.renderStrict
            . Pretty.layoutSmart Dhall.Pretty.layoutOpts
            . Dhall.Pretty.prettyExpr

    let interpret = do
            inputJSString <- getInput

            let inputString = Data.JSString.unpack inputJSString
            let inputText   = Data.Text.pack inputString

            case Dhall.Parser.exprFromText "(input)" inputText of
                Left exception -> do
                    errOutput exception
                Right parsedExpression -> do
                  eitherResolvedExpression <- Control.Exception.try (Dhall.Import.load parsedExpression)
                  case eitherResolvedExpression of
                      Left exception -> do
                          errOutput (exception :: SomeException)
                      Right resolvedExpression -> do
                          case Dhall.TypeCheck.typeOf resolvedExpression of
                              Left exception -> do
                                  errOutput exception
                              Right inferredType -> do
                                  mode <- Data.IORef.readIORef modeRef
                                  case mode of
                                      Dhall -> do
                                          let normalizedExpression =
                                                  Dhall.Core.normalize resolvedExpression
                                          let dhallText =
                                                  prettyExpression (Annot normalizedExpression inferredType)
                                          setOutput dhallText

                                      JSON -> do
                                          case Dhall.JSON.dhallToJSON resolvedExpression of
                                              Left exception -> do
                                                  errOutput exception
                                              Right value -> do
                                                  let jsonBytes = Data.Aeson.Encode.Pretty.encodePretty' jsonConfig value
                                                  case Data.Text.Lazy.Encoding.decodeUtf8' jsonBytes of
                                                      Left exception -> do
                                                          errOutput exception
                                                      Right jsonText -> do
                                                          setOutput (Data.Text.Lazy.toStrict jsonText)

    interpret

    interpretAsync <- GHCJS.Foreign.Callback.asyncCallback interpret

    registerInterpret interpretAsync

    let dhallOutput = do
            Data.IORef.writeIORef modeRef Dhall

            selectTab "mode-tab" "dhall-tab"

            interpret

    dhallOutputAsync <- GHCJS.Foreign.Callback.asyncCallback dhallOutput

    registerDhallOutput dhallOutputAsync

    let jsonOutput = do
            Data.IORef.writeIORef modeRef JSON

            selectTab "mode-tab" "json-tab"

            interpret

    jsonOutputAsync <- GHCJS.Foreign.Callback.asyncCallback jsonOutput

    registerJSONOutput jsonOutputAsync

    return ()
