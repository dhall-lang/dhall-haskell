{-# LANGUAGE OverloadedStrings #-}

module Dhall.Format ( format ) where

import Dhall.Parser (exprAndHeaderFromText)
import Dhall.Pretty (annToAnsiStyle, prettyExpr, layoutOpts)

import Data.Monoid ((<>))

import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Control.Exception
import qualified Data.Text.IO
import qualified System.Console.ANSI
import qualified System.IO

format :: Maybe FilePath -> IO ()
format inplace = do
        case inplace of
            Just file -> do
                text <- Data.Text.IO.readFile file
                (header, expr) <- case exprAndHeaderFromText "(stdin)" text of
                    Left  err -> Control.Exception.throwIO err
                    Right x   -> return x

                let doc = Pretty.pretty header <> Pretty.pretty expr
                System.IO.withFile file System.IO.WriteMode (\handle -> do
                    Pretty.renderIO handle (Pretty.layoutSmart layoutOpts doc)
                    Data.Text.IO.hPutStrLn handle "" )
            Nothing -> do
                inText <- Data.Text.IO.getContents

                (header, expr) <- case exprAndHeaderFromText "(stdin)" inText of
                    Left  err -> Control.Exception.throwIO err
                    Right x   -> return x

                let doc = Pretty.pretty header <> prettyExpr expr

                supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

                if supportsANSI
                  then
                    Pretty.renderIO
                      System.IO.stdout
                      (fmap annToAnsiStyle (Pretty.layoutSmart layoutOpts doc))
                  else
                    Pretty.renderIO
                      System.IO.stdout
                      (Pretty.layoutSmart layoutOpts (Pretty.unAnnotate doc))
