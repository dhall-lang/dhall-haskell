{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @dhall format@ subcommand

module Dhall.Format
    ( -- * Format
      Format(..)
    , format
    ) where

import Data.Monoid ((<>))
import Dhall.Pretty (CharacterSet(..), annToAnsiStyle)

import Dhall.Util
    ( Censor
    , Header(..)
    , Input(..)
    , InputMode(..)
    , NotModified(..)
    )

import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Control.Exception
import qualified Data.Text.IO
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified System.AtomicWrite.Writer.LazyText        as AtomicWrite.LazyText
import qualified System.Console.ANSI
import qualified System.IO

-- | Arguments to the `format` subcommand
data Format = Format
    { characterSet :: CharacterSet
    , censor       :: Censor
    , input        :: Input
    , inputMode    :: InputMode
    }

-- | Implementation of the @dhall format@ subcommand
format :: Format -> IO ()
format (Format {..}) = do
    let layoutHeaderAndExpr (Header header, expr) =
            Dhall.Pretty.layout
                (   Pretty.pretty header
                <>  Dhall.Pretty.prettyCharacterSet characterSet expr 
                <>  "\n")

    let layoutInput = do
            headerAndExpr <- Dhall.Util.getExpressionAndHeader censor input

            return (layoutHeaderAndExpr headerAndExpr)

    case inputMode of
        Modify -> do
            docStream <- layoutInput

            case input of
                InputFile file -> do
                    AtomicWrite.LazyText.atomicWriteFile
                        file
                        (Pretty.Text.renderLazy docStream)

                StandardInput -> do
                    supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

                    Pretty.Terminal.renderIO
                        System.IO.stdout
                        (if supportsANSI
                            then (fmap annToAnsiStyle docStream)
                            else (Pretty.unAnnotateS docStream))

        Check -> do
            originalText <- case input of
                InputFile file -> Data.Text.IO.readFile file
                StandardInput  -> Data.Text.IO.getContents

            docStream <- case input of
                InputFile _    -> layoutInput
                StandardInput  -> do
                    headerAndExpr <- Dhall.Util.getExpressionAndHeaderFromStdinText censor originalText
                    return (layoutHeaderAndExpr headerAndExpr)

            let formattedText = Pretty.Text.renderStrict docStream

            if originalText == formattedText
                then return ()
                else do
                    let command = "format"

                    let modified = "formatted"

                    Control.Exception.throwIO NotModified{..}
