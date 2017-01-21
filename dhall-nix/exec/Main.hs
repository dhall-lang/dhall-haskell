{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Exception (SomeException)
import Text.Trifecta.Delta (Delta(..))

import qualified Control.Exception
import qualified Data.Text.Lazy.IO
import qualified Dhall
import qualified Dhall.Import
import qualified Dhall.Nix
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Nix.Pretty
import qualified Options.Generic
import qualified System.Exit
import qualified System.IO

main :: IO ()
main = handle (Dhall.detailed (do
    () <- Options.Generic.getRecord "Compile Dhall to Nix"

    inText <- Data.Text.Lazy.IO.getContents

    expr <- case Dhall.Parser.exprFromText (Directed "(stdin)" 0 0 0 0) inText of
        Left  err  -> Control.Exception.throwIO err
        Right expr -> return expr

    expr' <- Dhall.Import.load expr
    case Dhall.TypeCheck.typeOf expr' of
        Left  err -> Control.Exception.throwIO err
        Right _   -> return ()

    nix <- case Dhall.Nix.dhallToNix expr' of
        Left err  -> Control.Exception.throwIO err
        Right nix -> return nix
    print (Nix.Pretty.prettyNix nix) ))

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
