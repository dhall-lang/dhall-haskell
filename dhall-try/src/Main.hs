{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.JSString
import qualified Data.Text
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified GHCJS.DOM
import qualified GHCJS.DOM.Document
import qualified GHCJS.DOM.Element
import qualified GHCJS.DOM.EventM
import qualified GHCJS.DOM.GlobalEventHandlers
import qualified GHCJS.DOM.HTMLCollection
import qualified GHCJS.DOM.HTMLElement
import qualified GHCJS.DOM.Types

import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Dhall.Import (ImportResolutionDisabled(..))
import GHCJS.DOM.HTMLDivElement (HTMLDivElement(..))
import GHCJS.DOM.HTMLElement (HTMLElement(..))
import GHCJS.DOM.Types (IsGObject, JSString, JSVal, MonadDOM, MonadJSM)

foreign import javascript unsafe "editor.getValue()" getEditorText :: IO JSString

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

    dhallInput  <- the HTMLDivElement "CodeMirror"
    dhallOutput <- the HTMLElement "dhall-output"

    let callback :: MonadDOM m => m ()
        callback = do
            inputJSString <- liftIO getEditorText -- GHCJS.DOM.HTMLElement.getInnerText dhallInput

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
                                    return (Dhall.Core.pretty normalizedExpression)

            GHCJS.DOM.HTMLElement.setInnerText dhallOutput (fixup outputText)

    callback

    _ <- GHCJS.DOM.EventM.on dhallInput GHCJS.DOM.GlobalEventHandlers.input callback

    return ()
