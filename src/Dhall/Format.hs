{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall format@ subcommand

module Dhall.Format
    ( -- * Format
      format
    ) where

import Dhall.Parser (exprAndHeaderFromText)
import Dhall.Pretty (CharacterSet(..), annToAnsiStyle, layoutOpts)

import Data.Monoid ((<>))

import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Control.Exception
import qualified Data.Text.IO
import qualified Dhall.Pretty
import qualified System.Console.ANSI
import qualified System.IO

-- | Implementation of the @dhall format@ subcommand
format
    :: CharacterSet
    -> Maybe FilePath
    -- ^ Modify file in-place if present, otherwise read from @stdin@ and write
    --   to @stdout@
    -> IO ()
format characterSet inplace = do
        case inplace of
            Just file -> do
                text <- Data.Text.IO.readFile file
                (header, expr) <- case exprAndHeaderFromText "(stdin)" text of
                    Left  err -> Control.Exception.throwIO err
                    Right x   -> return x

                let doc =   Pretty.pretty header
                        <>  fmap annToAnsiStyle (Dhall.Pretty.prettyCharacterSet characterSet expr)
                System.IO.withFile file System.IO.WriteMode (\handle -> do
                    Pretty.renderIO handle (Pretty.layoutSmart layoutOpts doc)
                    Data.Text.IO.hPutStrLn handle "" )
            Nothing -> do
                inText <- Data.Text.IO.getContents

                (header, expr) <- case exprAndHeaderFromText "(stdin)" inText of
                    Left  err -> Control.Exception.throwIO err
                    Right x   -> return x

                let doc =   Pretty.pretty header
                        <>  Dhall.Pretty.prettyCharacterSet characterSet expr

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
