{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Monoid (mempty)
import Data.Version (showVersion)
import Dhall.Core (pretty, normalize)
import Dhall.Import (Imported(..), hashExpressionToCode, load)
import Dhall.Parser (Src, exprFromText)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import Options.Generic (Generic, ParseRecord, type (<?>)(..))
import System.IO (stderr)
import System.Exit (exitFailure, exitSuccess)
import Text.Trifecta.Delta (Delta(..))

import qualified Paths_dhall as Meta

import qualified Control.Exception
import qualified Data.Text.Lazy.IO
import qualified Dhall.TypeCheck
import qualified Options.Generic
import qualified System.IO

data Options = Options
    { explain :: Bool <?> "Explain error messages in more detail"
    , version :: Bool <?> "Display version and exit"
    } deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
    options <- Options.Generic.getRecord "Compiler for the Dhall language"
    when (unHelpful (version options)) $ do
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
                if unHelpful (explain options)
                    then Control.Exception.throwIO (DetailedTypeError e)
                    else do
                        Data.Text.Lazy.IO.hPutStrLn stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler1 (Imported ps e) = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn stderr ""
                if unHelpful (explain options)
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.Lazy.IO.hPutStrLn stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handler2 e = do
                let _ = e :: SomeException
                System.IO.hPrint stderr e
                System.Exit.exitFailure

    handle (do
        inText <- Data.Text.Lazy.IO.getContents

        expr <- case exprFromText (Directed "(stdin)" 0 0 0 0) inText of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr

        expr' <- load expr

        _ <- case Dhall.TypeCheck.typeOf expr' of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()

        Data.Text.Lazy.IO.putStrLn (hashExpressionToCode (normalize expr')) )
