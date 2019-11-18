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
import Data.Monoid ((<>))
import Dhall.Pretty (CharacterSet(..), annToAnsiStyle)
import Dhall.Util (Censor, Input(..), Header(..))

import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Control.Exception
import qualified Data.Text.IO
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified System.Console.ANSI
import qualified System.IO

data NotFormatted = NotFormatted
    deriving (Exception)

instance Show NotFormatted where
    show _ = ""

-- | Arguments to the `format` subcommand
data Format = Format
    { characterSet :: CharacterSet
    , censor       :: Censor
    , formatMode   :: FormatMode
    }

{-| The `format` subcommand can either `Modify` its input or simply `Check`
    that the input is already formatted
-}
data FormatMode
    = Modify { inplace :: Input }
    | Check  { path :: Input }

-- | Implementation of the @dhall format@ subcommand
format
    :: Format
    -> IO ()
format (Format {..}) = do
    let layoutHeaderAndExpr (Header header, expr) =
            Dhall.Pretty.layout
                (   Pretty.pretty header
                <>  Dhall.Pretty.prettyCharacterSet characterSet expr 
                <>  "\n")

    let layoutInput input = do
            headerAndExpr <- Dhall.Util.getExpressionAndHeader censor input

            return (layoutHeaderAndExpr headerAndExpr)

    case formatMode of
        Modify {..} -> do
            docStream <- layoutInput inplace

            case inplace of
                InputFile file -> do
                    System.IO.withFile file System.IO.WriteMode (\handle -> do
                        Pretty.Terminal.renderIO handle (Pretty.unAnnotateS docStream))

                StandardInput -> do
                    supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

                    Pretty.Terminal.renderIO
                        System.IO.stdout
                        (if supportsANSI
                            then (fmap annToAnsiStyle docStream)
                            else (Pretty.unAnnotateS docStream))

        Check {..} -> do
            originalText <- case path of
                InputFile file -> Data.Text.IO.readFile file
                StandardInput  -> Data.Text.IO.getContents

            docStream <- case path of
                InputFile _    -> layoutInput path
                StandardInput  -> do
                    headerAndExpr <- Dhall.Util.getExpressionAndHeaderFromStdinText censor originalText
                    return (layoutHeaderAndExpr headerAndExpr)

            let formattedText = Pretty.Text.renderStrict docStream

            if originalText == formattedText
                then return ()
                else Control.Exception.throwIO NotFormatted
