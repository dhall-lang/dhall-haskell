{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Monoid (mempty)
import Data.Version (showVersion)
import Dhall.Core (normalize)
import Dhall.Import (Imported(..), load)
import Dhall.Parser (Src)
import Dhall.Pretty (annToAnsiStyle, prettyExpr)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import Options.Generic (Generic, ParseRecord, Wrapped, type (<?>)(..), (:::))
import System.Exit (exitFailure, exitSuccess)
import Text.Trifecta.Delta (Delta(..))

import qualified Paths_dhall as Meta

import qualified Control.Exception
import qualified Data.Text.Lazy.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Options.Generic
import qualified System.Console.ANSI
import qualified System.IO

data Options w = Options
    { explain :: w ::: Bool <?> "Explain error messages in more detail"
    , version :: w ::: Bool <?> "Display version and exit"
    , pretty  :: w ::: Bool <?> "Format output"
    } deriving (Generic)

instance ParseRecord (Options Wrapped)

opts :: Pretty.LayoutOptions
opts =
    Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

unbounded :: Pretty.LayoutOptions
unbounded = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.Unbounded }

main :: IO ()
main = do
    Options {..} <- Options.Generic.unwrapRecord "Compiler for the Dhall language"
    when version $ do
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
                if explain
                    then Control.Exception.throwIO (DetailedTypeError e)
                    else do
                        Data.Text.Lazy.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler1 (Imported ps e) = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.Lazy.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handler2 e = do
                let _ = e :: SomeException
                System.IO.hSetEncoding System.IO.stderr System.IO.utf8
                System.IO.hPrint System.IO.stderr e
                System.Exit.exitFailure

    handle (do
        System.IO.hSetEncoding System.IO.stdin System.IO.utf8
        inText <- Data.Text.Lazy.IO.getContents

        expr <- case Dhall.Parser.exprFromText (Directed "(stdin)" 0 0 0 0) inText of
            Left  err -> Control.Exception.throwIO err
            Right x   -> return x

        let render h e = do
                let doc = prettyExpr e

                let layoutOptions = if pretty then opts else unbounded
                let stream = Pretty.layoutSmart layoutOptions doc

                supportsANSI <- System.Console.ANSI.hSupportsANSI h
                let ansiStream =
                        if supportsANSI
                        then fmap annToAnsiStyle stream
                        else Pretty.unAnnotateS stream

                Pretty.renderIO h ansiStream
                Data.Text.Lazy.IO.hPutStrLn h ""


        expr' <- load expr

        typeExpr <- case Dhall.TypeCheck.typeOf expr' of
            Left  err      -> Control.Exception.throwIO err
            Right typeExpr -> return typeExpr

        render System.IO.stderr (normalize typeExpr)
        Data.Text.Lazy.IO.hPutStrLn System.IO.stderr mempty
        render System.IO.stdout (normalize expr') )
