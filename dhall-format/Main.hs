{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Version (showVersion)
import Dhall.Parser (exprFromText)
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

data Mode = Default | Resolve | TypeCheck | Normalize

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
            System.IO.hPrint stderr e
            System.Exit.exitFailure

    Control.Exception.handle handler (do
        case unHelpful (inplace options) of
            Just file -> do
                let fileString = Filesystem.Path.CurrentOS.encodeString file
                strictText <- Data.Text.IO.readFile fileString
                let lazyText = Data.Text.Lazy.fromStrict strictText
                expr <- case exprFromText (Directed "(stdin)" 0 0 0 0) lazyText of
                    Left  err  -> Control.Exception.throwIO err
                    Right expr -> return expr

                let doc = Pretty.pretty expr
                System.IO.withFile fileString System.IO.WriteMode (\handle -> do
                    Pretty.renderIO handle (Pretty.layoutSmart opts doc) )
            Nothing -> do
                inText <- Data.Text.Lazy.IO.getContents

                expr <- case exprFromText (Directed "(stdin)" 0 0 0 0) inText of
                    Left  err  -> Control.Exception.throwIO err
                    Right expr -> return expr

                let doc = Pretty.pretty expr
                Pretty.renderIO System.IO.stdout (Pretty.layoutSmart opts doc)
                Data.Text.IO.putStrLn "" )
