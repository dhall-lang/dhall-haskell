{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @dhall format@ subcommand

module Dhall.Format
    ( -- * Format
      Format(..)
    , format
    ) where

import Data.Foldable      (for_)
import Data.List.NonEmpty (NonEmpty)
import Dhall.Pretty       (annToAnsiStyle, detectCharacterSet)
import Dhall.Util
    ( Censor
    , CheckFailed (..)
    , Header (..)
    , Input (..)
    , OutputMode (..)
    , Transitivity (..)
    , handleMultipleChecksFailed
    )

import qualified Data.Text.IO
import qualified Dhall.Import
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified Prettyprinter                      as Pretty
import qualified Prettyprinter.Render.Terminal      as Pretty.Terminal
import qualified Prettyprinter.Render.Text          as Pretty.Text
import qualified System.AtomicWrite.Writer.LazyText as AtomicWrite.LazyText
import qualified System.Console.ANSI
import qualified System.FilePath
import qualified System.IO
import Dhall.Pretty.Internal        (ChooseCharacterSet(..), chooseCharsetOrUseDefault)

-- | Arguments to the `format` subcommand
data Format = Format
    { chosenCharacterSet :: ChooseCharacterSet
    , censor             :: Censor
    , transitivity       :: Transitivity
    , inputs             :: NonEmpty Input
    , outputMode         :: OutputMode
    }

-- | Implementation of the @dhall format@ subcommand
format :: Format -> IO ()
format (Format { inputs = inputs0, transitivity = transitivity0, ..}) =
    handleMultipleChecksFailed "format" "formatted" go inputs0
  where
    go input = do
        let directory = case input of
                StandardInput ->
                    "."
                InputFile file ->
                    System.FilePath.takeDirectory file

        let status = Dhall.Import.emptyStatus directory

        let layoutHeaderAndExpr (Header header, expr) =
                let characterSet = chooseCharsetOrUseDefault (detectCharacterSet expr) chosenCharacterSet
                in
                Dhall.Pretty.layout
                    (   Pretty.pretty header
                    <>  Dhall.Pretty.prettyCharacterSet characterSet expr
                    <>  "\n")

        (inputName, originalText, transitivity) <- case input of
            InputFile file -> do
                text <- Data.Text.IO.readFile file

                return (file, text, transitivity0)
            StandardInput -> do
                text <- Data.Text.IO.getContents

                return ("(input)", text, NonTransitive)


        headerAndExpr@(_, parsedExpression) <- Dhall.Util.getExpressionAndHeaderFromStdinText censor inputName originalText

        case transitivity of
            Transitive ->
                for_ parsedExpression $ \import_ -> do
                    maybeFilepath <- Dhall.Import.dependencyToFile status import_

                    for_ maybeFilepath $ \filepath ->
                        go (InputFile filepath)

            NonTransitive ->
                return ()

        let docStream = layoutHeaderAndExpr headerAndExpr

        let formattedText = Pretty.Text.renderStrict docStream

        case outputMode of
            Write -> do
                case input of
                    InputFile file ->
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

                return (Right ())

            Check ->
                return $
                    if originalText == formattedText
                        then Right ()
                        else Left CheckFailed{..}
