{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @dhall format@ subcommand

module Dhall.Format
    ( -- * Format
      Format(..)
    , format
    ) where

import Data.Foldable (for_, toList)
import Data.Monoid ((<>))
import Dhall.Pretty (CharacterSet(..), annToAnsiStyle)

import Dhall.Util
    ( Censor
    , CheckFailed(..)
    , Header(..)
    , OutputMode(..)
    , PossiblyTransitiveInput(..)
    , Transitivity(..)
    )

import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Control.Exception
import qualified Data.Text.IO
import qualified Dhall.Import
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified System.AtomicWrite.Writer.LazyText        as AtomicWrite.LazyText
import qualified System.FilePath
import qualified System.Console.ANSI
import qualified System.IO

-- | Arguments to the `format` subcommand
data Format = Format
    { characterSet :: CharacterSet
    , censor       :: Censor
    , input        :: PossiblyTransitiveInput
    , outputMode   :: OutputMode
    }

-- | Implementation of the @dhall format@ subcommand
format :: Format -> IO ()
format (Format { input = input0, ..}) = go input0
  where
    go input = do
        let directory = case input of
                NonTransitiveStandardInput ->
                    "."
                PossiblyTransitiveInputFile file _ ->
                    System.FilePath.takeDirectory file

        let status = Dhall.Import.emptyStatus directory

        let layoutHeaderAndExpr (Header header, expr) =
                Dhall.Pretty.layout
                    (   Pretty.pretty header
                    <>  Dhall.Pretty.prettyCharacterSet characterSet expr
                    <>  "\n")

        (originalText, transitivity) <- case input of
            PossiblyTransitiveInputFile file transitivity -> do
                text <- Data.Text.IO.readFile file

                return (text, transitivity)

            NonTransitiveStandardInput -> do
                text <- Data.Text.IO.getContents

                return (text, NonTransitive)

        headerAndExpr@(_, parsedExpression) <- Dhall.Util.getExpressionAndHeaderFromStdinText censor originalText

        case transitivity of
            Transitive -> do
                for_ (toList parsedExpression) $ \import_ -> do
                    maybeFilepath <- Dhall.Import.dependencyToFile status import_

                    for_ maybeFilepath $ \filepath -> do
                        go (PossiblyTransitiveInputFile filepath Transitive)

            NonTransitive ->
                return ()

        let docStream = layoutHeaderAndExpr headerAndExpr

        let formattedText = Pretty.Text.renderStrict docStream

        case outputMode of
            Write -> do
                case input of
                    PossiblyTransitiveInputFile file _ -> do
                        if originalText == formattedText
                            then return ()
                            else AtomicWrite.LazyText.atomicWriteFile
                                    file
                                    (Pretty.Text.renderLazy docStream)

                    NonTransitiveStandardInput -> do
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
