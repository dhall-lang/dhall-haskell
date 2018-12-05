{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.JSString
import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified GHCJS.Foreign.Callback

import Data.JSString (JSString)
import Data.Text (Text)
import Dhall.Core (Expr(..))
import Dhall.Import (ImportResolutionDisabled(..))
import GHCJS.Foreign.Callback (Callback)

foreign import javascript unsafe "input.getValue()" getInput :: IO JSString

foreign import javascript unsafe "input.on('change', $1)" registerCallback :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "output.setValue($1)" setOutput :: JSString -> IO ()

fixup :: Text -> Text
fixup = Data.Text.replace "\ESC[1;31mError\ESC[0m" "Error"

main :: IO ()
main = do
    let prettyExpression =
              Pretty.renderStrict
            . Pretty.layoutSmart Dhall.Pretty.layoutOpts
            . Dhall.Pretty.prettyExpr

    let callback :: IO ()
        callback = do
            inputJSString <- getInput

            let inputString = Data.JSString.unpack inputJSString
            let inputText   = Data.Text.pack inputString

            outputText <- case Dhall.Parser.exprFromText "(input)" inputText of
                Left exception -> do
                    return (Data.Text.pack (show exception))
                Right parsedExpression -> do
                    case traverse (\_ -> Nothing) parsedExpression of
                        Nothing -> do
                            return (Data.Text.pack (show ImportResolutionDisabled))
                        Just resolvedExpression -> do
                            case Dhall.TypeCheck.typeOf resolvedExpression of
                                Left exception -> do
                                    return (Data.Text.pack (show exception))
                                Right inferredType -> do
                                    let normalizedExpression =
                                            Dhall.Core.normalize resolvedExpression
                                    return (prettyExpression (Annot normalizedExpression inferredType))

            let outputString   = Data.Text.unpack (fixup outputText)
            let outputJSString = Data.JSString.pack outputString

            setOutput outputJSString

    callback

    async <- GHCJS.Foreign.Callback.asyncCallback callback

    registerCallback async

    return ()
