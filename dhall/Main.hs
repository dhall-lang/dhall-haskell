{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Monoid (mempty, (<>))
import Data.Version (showVersion)
import Dhall.Core (normalize)
import Dhall.Import (Imported(..), load)
import Dhall.Parser (Src, exprAndHeaderFromText)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import Options.Generic (Generic, ParseRecord, type (<?>)(..))
import System.Exit (exitFailure, exitSuccess)
import Text.Trifecta.Delta (Delta(..))

import qualified Paths_dhall as Meta

import qualified Control.Exception
import qualified Data.Text.Lazy.IO
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall.Core
import qualified Dhall.TypeCheck
import qualified Options.Generic
import qualified System.IO

data Options = Options
    { explain :: Bool <?> "Explain error messages in more detail"
    , version :: Bool <?> "Display version and exit"
    , pretty  :: Bool <?> "Format output"
    } deriving (Generic)

instance ParseRecord Options

opts :: Pretty.LayoutOptions
opts =
    Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

main :: IO ()
main = do
    options <- Options.Generic.getRecord "Compiler for the Dhall language"
    when (unHelpful (version options)) $ do
      putStrLn (showVersion Meta.version)
      exitSuccess

    let handle =
                Control.Exception.handle handler2
            .   Control.Exception.handle handler1
            .   Control.Exception.handle handler0
          where
            handler0 e = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if unHelpful (explain options)
                    then Control.Exception.throwIO (DetailedTypeError e)
                    else do
                        Data.Text.Lazy.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler1 (Imported ps e) = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if unHelpful (explain options)
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.Lazy.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handler2 e = do
                let _ = e :: SomeException
                System.IO.hPrint System.IO.stderr e
                System.Exit.exitFailure

    handle (do
        inText <- Data.Text.Lazy.IO.getContents

        (header, expr) <- case exprAndHeaderFromText (Directed "(stdin)" 0 0 0 0) inText of
            Left  err -> Control.Exception.throwIO err
            Right x   -> return x

        let render h e =
                if unHelpful (pretty options)
                then do
                    let doc = Pretty.pretty header <> Pretty.pretty e
                    Pretty.renderIO h (Pretty.layoutSmart opts doc)
                    Data.Text.Lazy.IO.hPutStrLn h ""
                else do
                    Data.Text.Lazy.IO.hPutStrLn h (Dhall.Core.pretty e)

        expr' <- load expr

        typeExpr <- case Dhall.TypeCheck.typeOf expr' of
            Left  err      -> Control.Exception.throwIO err
            Right typeExpr -> return typeExpr
        render System.IO.stderr (normalize typeExpr)
        Data.Text.Lazy.IO.hPutStrLn System.IO.stderr mempty
        render System.IO.stdout (normalize expr') )
