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
import qualified GHCJS.DOM
import qualified GHCJS.DOM.Document
import qualified GHCJS.DOM.Element
import qualified GHCJS.DOM.HTMLCollection
import qualified GHCJS.DOM.HTMLElement
import qualified GHCJS.DOM.Types
import qualified GHCJS.Foreign.Callback

import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Dhall.Import (ImportResolutionDisabled(..))
import GHCJS.DOM.HTMLElement (HTMLElement(..))
import GHCJS.DOM.Types (IsGObject, JSString, JSVal, MonadDOM, MonadJSM)
import GHCJS.Foreign.Callback (Callback)

foreign import javascript unsafe "editor.getValue()" getEditorText :: IO JSString

foreign import javascript unsafe "editor.on('change', $1)" registerCallback :: Callback (IO ()) -> IO ()

orDie :: MonadFail m => m (Maybe a) -> String -> m a
m `orDie` string = do
    x <- m
    case x of
        Nothing -> fail string
        Just y  -> return y

fixup :: Text -> Text
fixup = Data.Text.replace "\ESC[1;31mError\ESC[0m" "Error"

main :: IO ()
main = do
    document <- GHCJS.DOM.currentDocument
        `orDie` "Unable to get the current document"

    let the :: (IsGObject a, MonadFail m, MonadJSM m)
            => (JSVal -> a) -> String -> m a
        the elementType className = do
                elements <- GHCJS.DOM.Document.getElementsByClassName document className

                element <- GHCJS.DOM.HTMLCollection.item elements 0
                    `orDie` ("Unable to locate an element with a class name of `" ++ className ++ "`")

                GHCJS.DOM.Types.castTo elementType element
                    `orDie` ("The first element with a class name of `" ++ className ++ "` was not the right element type")

    dhallOutput <- the HTMLElement "dhall-output"

    let prettyExpression =
              Pretty.renderStrict
            . Pretty.layoutSmart Dhall.Pretty.layoutOpts
            . Dhall.Pretty.prettyExpr

    let callback :: MonadDOM m => m ()
        callback = do
            inputJSString <- liftIO getEditorText

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
                                Right _ -> do
                                    let normalizedExpression =
                                            Dhall.Core.normalize resolvedExpression
                                    return (prettyExpression normalizedExpression)

            GHCJS.DOM.HTMLElement.setInnerText dhallOutput (fixup outputText)

    callback

    async <- GHCJS.Foreign.Callback.asyncCallback callback

    registerCallback async

    return ()
