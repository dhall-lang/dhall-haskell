{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Options.Generic (Generic, ParseRecord, type (<?>)(..))
import System.Exit (ExitCode(..))

import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.Bash
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified Options.Generic
import qualified System.Exit
import qualified System.IO

data Options = Options
    { explain :: Bool
        <?> "Explain error messages in detail"
    , declare :: Maybe ByteString
        <?> "Declare the given variable as a statement instead of an expression"
    } deriving (Generic, ParseRecord)

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
    Options {..} <- Options.Generic.getRecord "Compile Dhall to Bash"

    (if unHelpful explain then Dhall.detailed else id) (handle (do
        inText <- Data.Text.IO.getContents

        expr <- case Dhall.Parser.exprFromText "(stdin)" inText of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr

        expr' <- Dhall.Import.load expr
        case Dhall.TypeCheck.typeOf expr' of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()

        bytes <- case unHelpful declare of
            Nothing  -> do
                case Dhall.Bash.dhallToExpression expr' of
                    Left  err   -> Control.Exception.throwIO err
                    Right bytes -> return bytes
            Just var -> do
                case Dhall.Bash.dhallToStatement expr' var of
                    Left  err   -> Control.Exception.throwIO err
                    Right bytes -> return bytes
        Data.ByteString.putStr bytes ))

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
