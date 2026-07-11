{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Exception (SomeException)
import Data.Text         (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics      (Generic)
import Options.Applicative
import System.Exit       (ExitCode (..))

import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.Text.IO
import qualified Data.Version
import qualified Dhall
import qualified Dhall.Bash
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified Paths_dhall_bash
import qualified System.Exit
import qualified System.IO

data Options = Options
    { explain :: Bool
    , declare :: Maybe Text
    , version :: Bool
    } deriving (Generic)

-- | Parser for all the command arguments and options
parseOptions :: Parser Options
parseOptions = do
    explain <- switch
        ( long "explain"
        <> help "Explain error messages in detail"
        )

    declare <- (optional . strOption)
        ( long "declare"
        <> metavar "NAME"
        <> help "Declare the given variable as a statement instead of an expression"
        )

    version <- switch
        ( long "version"
        <> help "Display version"
        )

    pure Options{..}

parserInfo :: ParserInfo Options
parserInfo =
    info
        (helper <*> parseOptions)
        (   fullDesc
        <>  progDesc "Compile Dhall to Bash"
        )

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
    Options {..} <- execParser parserInfo

    if version
        then do
            putStrLn (Data.Version.showVersion Paths_dhall_bash.version)
            System.Exit.exitSuccess
        else return ()

    (if explain then Dhall.detailed else id) (handle (do
        inText <- Data.Text.IO.getContents

        expr <- case Dhall.Parser.exprFromText "(input)" inText of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr

        expr' <- Dhall.Import.load expr
        case Dhall.TypeCheck.typeOf expr' of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()

        bytes <- case declare of
            Nothing  -> do
                case Dhall.Bash.dhallToExpression expr' of
                    Left  err   -> Control.Exception.throwIO err
                    Right bytes -> return bytes
            Just var -> do
                case Dhall.Bash.dhallToStatement expr' (encodeUtf8 var) of
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
