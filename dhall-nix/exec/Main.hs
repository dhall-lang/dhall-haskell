{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Exception (SomeException)
import Options.Generic   (Generic, ParseRecord, type (<?>) (..))
import System.Exit       (ExitCode (..))

import qualified Control.Exception
import qualified Data.Text.IO
import qualified Data.Version
import qualified Dhall
import qualified Dhall.Import
import qualified Dhall.Nix
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified Nix.Pretty
import qualified Options.Generic
import qualified Paths_dhall_nix
import qualified System.Exit
import qualified System.IO

data Options = Options
    { version :: Bool
        <?> "Display version"
    } deriving (Generic, ParseRecord)

main :: IO ()
main = handle (Dhall.detailed (do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
    Options{..} <- Options.Generic.getRecord "Compile Dhall to Nix"

    if unHelpful version
        then do
            putStrLn (Data.Version.showVersion Paths_dhall_nix.version)
            System.Exit.exitSuccess
        else return ()

    inText <- Data.Text.IO.getContents

    expr <- case Dhall.Parser.exprFromText "(input)" inText of
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
    handler e = case Control.Exception.fromException e of
        Just ExitSuccess -> do
            Control.Exception.throwIO e
        _ -> do
            System.IO.hPutStrLn System.IO.stderr ""
            System.IO.hPrint    System.IO.stderr e
            System.Exit.exitFailure
