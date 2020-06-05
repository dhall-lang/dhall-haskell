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
    , CheckFailed(..)
    , Header(..)
    , Input(..)
    , OutputMode(..)
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
    , outputMode   :: OutputMode
    }

-- | Implementation of the @dhall format@ subcommand
format :: Format -> IO ()
format (Format {..}) = do
    let layoutHeaderAndExpr (Header header, expr) =
            Dhall.Pretty.layout
                (   Pretty.pretty header
                <>  Dhall.Pretty.prettyCharacterSet characterSet expr 
                <>  "\n")

    originalText <- case input of
        InputFile file -> Data.Text.IO.readFile file
        StandardInput  -> Data.Text.IO.getContents

    headerAndExpr <- Dhall.Util.getExpressionAndHeaderFromStdinText censor originalText

    let docStream = layoutHeaderAndExpr headerAndExpr

    let formattedText = Pretty.Text.renderStrict docStream

    case outputMode of
        Write -> do
            case input of
                InputFile file -> do
                    if originalText == formattedText
                        then return ()
                        else AtomicWrite.LazyText.atomicWriteFile
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
            if originalText == formattedText
                then return ()
                else do
                    let command = "format"

                    let modified = "formatted"

                    Control.Exception.throwIO CheckFailed{..}
