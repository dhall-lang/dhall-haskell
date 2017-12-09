{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

{-| Utility executable for pretty-printing Dhall code

    You typically want to use this to either:

    * improve the readability of Dhall code (either written or generated)
    * automatically format your Dhall code to avoid stylistic debates

    Note that this does not yet support:

    * Preserving all comments
        * Currently, this only preserves all leading comments and whitespace
          up until the last newline preceding the code
        * This lets you preserve a comment header but if you want to document
          subexpressions then you will need to split them into a separate
          file for now
    * Preserving multi-line strings (this reduces them to ordinary strings)
    * Preserving string interpolation (this expands interpolation to @++@)

    See the @Dhall.Tutorial@ module for example usage
-}
module Main where

import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Dhall.Parser (exprAndHeaderFromText)
import Filesystem.Path.CurrentOS (FilePath)
import Options.Generic (Generic, ParseRecord, type (<?>)(..))
import Prelude hiding (FilePath)
import System.IO (stderr)
import System.Exit (exitFailure, exitSuccess)
import Text.Trifecta.Delta (Delta(..))

import qualified Paths_dhall as Meta

import qualified Control.Exception
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Filesystem.Path.CurrentOS
import qualified Options.Generic
import qualified System.IO

data Options = Options
    { version :: Bool           <?> "Display version and exit"
    , inplace :: Maybe FilePath <?> "Modify the specified file in-place"
    } deriving (Generic)

instance ParseRecord Options

opts :: Pretty.LayoutOptions
opts =
    Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

main :: IO ()
main = do
    options <- Options.Generic.getRecord "Formatter for the Dhall language"
    when (unHelpful (version options)) $ do
      putStrLn (showVersion Meta.version)
      exitSuccess

    let handler e = do
            let _ = e :: SomeException
            System.IO.hSetEncoding System.IO.stderr System.IO.utf8
            System.IO.hPrint stderr e
            System.Exit.exitFailure

    Control.Exception.handle handler (do
        case unHelpful (inplace options) of
            Just file -> do
                let fileString = Filesystem.Path.CurrentOS.encodeString file
                strictText <- Data.Text.IO.readFile fileString
                let lazyText = Data.Text.Lazy.fromStrict strictText
                (header, expr) <- case exprAndHeaderFromText (Directed "(stdin)" 0 0 0 0) lazyText of
                    Left  err -> Control.Exception.throwIO err
                    Right x   -> return x

                let doc = Pretty.pretty header <> Pretty.pretty expr
                System.IO.withFile fileString System.IO.WriteMode (\handle -> do
                    Pretty.renderIO handle (Pretty.layoutSmart opts doc)
                    Data.Text.IO.hPutStrLn handle "" )
            Nothing -> do
                inText <- Data.Text.Lazy.IO.getContents

                (header, expr) <- case exprAndHeaderFromText (Directed "(stdin)" 0 0 0 0) inText of
                    Left  err -> Control.Exception.throwIO err
                    Right x   -> return x

                let doc = Pretty.pretty header <> Pretty.pretty expr
                Pretty.renderIO System.IO.stdout (Pretty.layoutSmart opts doc)
                Data.Text.IO.putStrLn "" )
