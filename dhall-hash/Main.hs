{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Dhall.Import (Imported(..), hashExpressionToCode, load)
import Dhall.Parser (Src, exprFromText)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import Options.Applicative (Parser, ParserInfo)
import System.IO (stderr)
import System.Exit (exitFailure, exitSuccess)

import qualified Paths_dhall as Meta

import qualified Control.Exception
import qualified Data.Text.IO
import qualified Dhall.TypeCheck
import qualified Options.Applicative
import qualified System.IO

data Options = Options
    { explain :: Bool
    , version :: Bool
    }

parseOptions :: Parser Options
parseOptions = Options <$> parseExplain <*> parseVersion
  where
    parseExplain =
        Options.Applicative.switch
            (   Options.Applicative.long "explain"
            <>  Options.Applicative.help "Explain error messages in more detail"
            )

    parseVersion =
        Options.Applicative.switch
            (   Options.Applicative.long "version"
            <>  Options.Applicative.help "Display version and exit"
            )

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.progDesc "Compute semantic hashes for Dhall expressions"
        <>  Options.Applicative.fullDesc
        )

main :: IO ()
main = do
    Options {..} <- Options.Applicative.execParser parserInfo

    when version $ do
      putStrLn (showVersion Meta.version)
      exitSuccess
    let handle =
                Control.Exception.handle handler2
            .   Control.Exception.handle handler1
            .   Control.Exception.handle handler0
          where
            handler0 e = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn stderr ""
                if explain
                    then Control.Exception.throwIO (DetailedTypeError e)
                    else do
                        Data.Text.IO.hPutStrLn stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler1 (Imported ps e) = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn stderr ""
                if explain
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.IO.hPutStrLn stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handler2 e = do
                let _ = e :: SomeException
                System.IO.hSetEncoding System.IO.stderr System.IO.utf8
                System.IO.hPrint stderr e
                System.Exit.exitFailure

    handle (do
        System.IO.hSetEncoding System.IO.stdin System.IO.utf8
        inText <- Data.Text.IO.getContents

        expr <- case exprFromText "(stdin)" inText of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr

        expr' <- load expr

        _ <- case Dhall.TypeCheck.typeOf expr' of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()

        Data.Text.IO.putStrLn (hashExpressionToCode expr') )
