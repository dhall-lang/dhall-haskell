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
import Control.Monad.IO.Class (MonadIO)
import Dhall.Parser (exprAndHeaderFromText)
import Dhall.Pretty (CharacterSet(..), annToAnsiStyle, layoutOpts)

import Data.Monoid ((<>))

import qualified Data.Text
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
                    prettyPrinted <- Data.Text.IO.readFile file >>= prettyExprSource

                    System.IO.withFile file System.IO.WriteMode (\handle -> do
                        Pretty.Terminal.renderIO handle prettyPrinted)
                Nothing -> do
                    prettyPrinted <- Data.Text.IO.getContents >>= prettyExprSource

                    supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

                    Pretty.Terminal.renderIO
                      System.IO.stdout
                      (if supportsANSI
                           then fmap annToAnsiStyle prettyPrinted
                           else Pretty.unAnnotateS prettyPrinted)
        Check {..} -> do
            originalText <- case path of
                Just file -> Data.Text.IO.readFile file
                Nothing   -> Data.Text.IO.getContents

            prettyPrinted <- prettyExprSource originalText

            let formattedText =
                    Pretty.Text.renderStrict prettyPrinted

            if originalText == formattedText
                then return ()
                else Control.Exception.throwIO NotFormatted

  where

    prettyExprSource :: MonadIO m => Data.Text.Text -> m (Pretty.SimpleDocStream ann)
    prettyExprSource text = do
        (header, expr) <- Dhall.Core.throws (exprAndHeaderFromText "(stdin)" text)
    
        let doc =   Pretty.pretty header
                <>  Pretty.unAnnotate (Dhall.Pretty.prettyCharacterSet characterSet expr)
                                <>  "\n"
    
        return (Pretty.removeTrailingWhitespace (Pretty.layoutSmart layoutOpts doc))
