{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson.Encode.Pretty
import qualified Data.IORef
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.JSON.Yaml
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Prettyprinter.Render.Text as Pretty

import Control.Exception           (Exception, SomeException)
import Data.Text                   (Text)
import GHC.JS.Foreign.Callback     (Callback)
import qualified GHC.JS.Foreign.Callback
import GHC.JS.Prim                 (JSVal, fromJSString, toJSString)

foreign import javascript unsafe "(() => input.getValue())"
  getInput :: IO JSVal

foreign import javascript unsafe "((f) => { input.on('change', f); })"
  registerInterpret :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "((f) => { dhallTab.onclick = f; })"
  registerDhallOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "((f) => { jsonTab.onclick = f; })"
  registerJSONOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "((f) => { yamlTab.onclick = f; })"
  registerYAMLOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "((f) => { typeTab.onclick = f; })"
  registerTypeOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "((f) => { hashTab.onclick = f; })"
  registerHashOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "((s) => { output.setValue(s); })"
  setOutput_ :: JSVal -> IO ()

foreign import javascript unsafe "((s) => { output.setOption('mode', s); })"
  setMode_ :: JSVal -> IO ()

foreign import javascript unsafe "((group, name) => { selectTab(group, name); })"
  selectTab :: JSVal -> JSVal -> IO ()

fixup :: Text -> Text
fixup = Data.Text.replace "\ESC[1;31mError\ESC[0m" "Error"

setOutput :: Text -> IO ()
setOutput = setOutput_ . toJSString . Data.Text.unpack

errOutput :: Exception e => e -> IO ()
errOutput = setOutput . fixup . Data.Text.pack . show

setMode :: Mode -> IO ()
setMode Dhall = setMode_ (toJSString "haskell")
setMode Type  = setMode_ (toJSString "haskell")
setMode JSON  = setMode_ (toJSString "javascript")
setMode YAML  = setMode_ (toJSString "yaml")
setMode Hash  = setMode_ (toJSString "null")

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

data Mode = Dhall | Type | JSON | YAML | Hash deriving (Show)

main :: IO ()
main = do
    modeRef <- Data.IORef.newIORef YAML

    let prettyExpression =
              Pretty.renderStrict
            . Dhall.Pretty.layout
            . Dhall.Pretty.prettyExpr

    let interpret = do
            inputJSVal <- getInput

            let inputString = fromJSString inputJSVal
            let inputText   = Data.Text.pack inputString

            case Dhall.Parser.exprFromText "(input)" inputText of
                Left exception ->
                    errOutput exception
                Right parsedExpression -> do
                  eitherResolvedExpression <- Control.Exception.try (Dhall.Import.load parsedExpression)
                  case eitherResolvedExpression of
                      Left exception ->
                          errOutput (exception :: SomeException)
                      Right resolvedExpression ->
                          case Dhall.TypeCheck.typeOf resolvedExpression of
                              Left exception ->
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

                                      JSON ->
                                          case Dhall.JSON.dhallToJSON resolvedExpression of
                                              Left exception ->
                                                  errOutput exception
                                              Right value -> do
                                                  let jsonBytes = Data.Aeson.Encode.Pretty.encodePretty' jsonConfig value
                                                  case Data.Text.Lazy.Encoding.decodeUtf8' jsonBytes of
                                                      Left exception ->
                                                          errOutput exception
                                                      Right jsonText ->
                                                          setOutput (Data.Text.Lazy.toStrict jsonText)
                                      YAML ->
                                          case Dhall.JSON.dhallToJSON resolvedExpression of
                                              Left exception ->
                                                  errOutput exception
                                              Right value -> do
                                                  let yamlBytes = Dhall.JSON.Yaml.jsonToYaml value False False
                                                  case Data.Text.Encoding.decodeUtf8' yamlBytes of
                                                      Left exception ->
                                                          errOutput exception
                                                      Right yamlText ->
                                                          setOutput yamlText

                                      Hash ->
                                          setOutput (Dhall.Import.hashExpressionToCode (Dhall.Core.alphaNormalize (Dhall.Core.normalize resolvedExpression)))

    interpret

    interpretAsync <- GHC.JS.Foreign.Callback.asyncCallback interpret

    registerInterpret interpretAsync

    let registerTabCallback mode tabName registerCallback = do
            let callback = do
                    Data.IORef.writeIORef modeRef mode

                    selectTab (toJSString "mode-tab") (toJSString tabName)

                    setMode mode

                    interpret

            callbackAsync <- GHC.JS.Foreign.Callback.asyncCallback callback

            registerCallback callbackAsync

    registerTabCallback Dhall "dhall-tab" registerDhallOutput
    registerTabCallback Type  "type-tab"  registerTypeOutput
    registerTabCallback JSON  "json-tab"  registerJSONOutput
    registerTabCallback YAML  "yaml-tab"  registerYAMLOutput
    registerTabCallback Hash  "hash-tab"  registerHashOutput
