{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @dhall format@ subcommand

module Dhall.Format
    ( -- * Format
      Format(..)
    , FormatMode(..)
    , format
    ) where

import Control.Exception (Exception)
import Dhall.Parser (exprAndHeaderFromText)
import Dhall.Pretty (CharacterSet(..), annToAnsiStyle, layoutOpts)

import Data.Monoid ((<>))

import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Control.Exception
import qualified Data.Text.IO
import qualified Dhall.Core
import qualified Dhall.Pretty
import qualified System.Console.ANSI
import qualified System.IO

data NotFormatted = NotFormatted
    deriving (Exception)

instance Show NotFormatted where
    show _ = ""

-- | Arguments to the `format` subcommand
data Format = Format
    { characterSet :: CharacterSet
    , formatMode   :: FormatMode
    }

{-| The `format` subcommand can either `Modify` its input or simply `Check`
    that the input is already formatted
-}
data FormatMode
    = Modify
        { inplace :: Maybe FilePath
          -- ^ Modify file in-place if present, otherwise read from @stdin@ and
          --   write to @stdout@
        }
    | Check
        { path :: Maybe FilePath
          -- ^ Read from the given file if present, otherwise read from @stdin@
        }

-- | Implementation of the @dhall format@ subcommand
format
    :: Format
    -> IO ()
format (Format {..}) =
    case formatMode of
        Modify {..} ->
            case inplace of
                Just file -> do
                    text <- Data.Text.IO.readFile file

                    (header, expr) <- Dhall.Core.throws (exprAndHeaderFromText "(stdin)" text)

                    let doc =   Pretty.pretty header
                            <>  Pretty.unAnnotate (Dhall.Pretty.prettyCharacterSet characterSet expr)
                            <>  "\n"
                    System.IO.withFile file System.IO.WriteMode (\handle -> do
                        Pretty.Terminal.renderIO handle (Pretty.layoutSmart layoutOpts doc))
                Nothing -> do
                    inText <- Data.Text.IO.getContents

                    (header, expr) <- Dhall.Core.throws (exprAndHeaderFromText "(stdin)" inText)

                    let doc =   Pretty.pretty header
                            <>  Dhall.Pretty.prettyCharacterSet characterSet expr
                            <>  "\n"

                    supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

                    if supportsANSI
                      then
                        Pretty.Terminal.renderIO
                          System.IO.stdout
                          (fmap annToAnsiStyle (Pretty.layoutSmart layoutOpts doc))
                      else
                        Pretty.Terminal.renderIO
                          System.IO.stdout
                          (Pretty.layoutSmart layoutOpts (Pretty.unAnnotate doc))
        Check {..} -> do
            originalText <- case path of
                Just file -> Data.Text.IO.readFile file
                Nothing   -> Data.Text.IO.getContents

            (header, expr) <- Dhall.Core.throws (exprAndHeaderFromText "(stdin)" originalText)

            let doc =   Pretty.pretty header
                    <>  Pretty.unAnnotate (Dhall.Pretty.prettyCharacterSet characterSet expr)
                    <>  "\n"

            let formattedText =
                    Pretty.Text.renderStrict (Pretty.layoutSmart layoutOpts doc)

            if originalText == formattedText
                then return ()
                else Control.Exception.throwIO NotFormatted
