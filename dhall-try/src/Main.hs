{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson.Encode.Pretty
import qualified Data.Char
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
import GHCJS.Foreign.Callback (Callback)

-- Work around the `yaml` package not working for GHCJS
foreign import javascript unsafe "jsonToYaml($1)" jsonToYaml_ :: JSString -> JSString

foreign import javascript unsafe "input.getValue()" getInput :: IO JSString

foreign import javascript unsafe "input.on('change', $1)" registerInterpret :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "dhallTab.onclick = $1" registerDhallOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "jsonTab.onclick = $1" registerJSONOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "yamlTab.onclick = $1" registerYAMLOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "typeTab.onclick = $1" registerTypeOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "output.setValue($1)" setOutput_ :: JSString -> IO ()

foreign import javascript unsafe "output.setOption('mode', $1)" setMode_ :: JSString -> IO ()

foreign import javascript unsafe "selectTab($1, $2)" selectTab :: JSString -> JSString -> IO ()

fixup :: Text -> Text
fixup = Data.Text.replace "\ESC[1;31mError\ESC[0m" "Error"

setOutput :: Text -> IO ()
setOutput = setOutput_ . Data.JSString.pack . Data.Text.unpack

errOutput :: Exception e => e -> IO ()
errOutput = setOutput . fixup . Data.Text.pack . show

setMode :: Mode -> IO ()
setMode Dhall = setMode_ "haskell"
setMode Type  = setMode_ "haskell"
setMode JSON  = setMode_ "javascript"
setMode YAML  = setMode_ "yaml"

jsonToYaml :: Text -> Text
jsonToYaml =
        Data.Text.dropWhileEnd Data.Char.isSpace
    .   Data.Text.pack
    .   Data.JSString.unpack
    .   jsonToYaml_
    .   Data.JSString.pack
    .   Data.Text.unpack

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

data Mode = Dhall | Type | JSON | YAML deriving (Show)

main :: IO ()
main = do
    modeRef <- Data.IORef.newIORef YAML

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
                                                  prettyExpression normalizedExpression
                                          setOutput dhallText

                                      Type -> do
                                          let typeText =
                                                  prettyExpression inferredType

                                          setOutput typeText

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
                                      YAML -> do
                                          case Dhall.JSON.dhallToJSON resolvedExpression of
                                              Left exception -> do
                                                  errOutput exception
                                              Right value -> do
                                                  let jsonBytes = Data.Aeson.Encode.Pretty.encodePretty' jsonConfig value
                                                  case Data.Text.Lazy.Encoding.decodeUtf8' jsonBytes of
                                                      Left exception -> do
                                                          errOutput exception
                                                      Right jsonText -> do
                                                          setOutput (jsonToYaml (Data.Text.Lazy.toStrict jsonText))

    interpret

    interpretAsync <- GHCJS.Foreign.Callback.asyncCallback interpret

    registerInterpret interpretAsync

    let registerTabCallback mode tabName registerCallback = do
            let callback = do
                    Data.IORef.writeIORef modeRef mode

                    selectTab "mode-tab" tabName

                    setMode mode

                    interpret

            callbackAsync <- GHCJS.Foreign.Callback.asyncCallback callback

            registerCallback callbackAsync

    registerTabCallback Dhall "dhall-tab" registerDhallOutput
    registerTabCallback Type  "type-tab"  registerTypeOutput
    registerTabCallback JSON  "json-tab"  registerJSONOutput
    registerTabCallback YAML  "yaml-tab"  registerYAMLOutput
