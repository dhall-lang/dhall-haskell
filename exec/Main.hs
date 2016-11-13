{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Control.Exception (SomeException)
import Data.Monoid (mempty)
import Data.Traversable
import Dhall.Core (pretty, normalize)
import Dhall.Import (Imported(..), load)
import Dhall.Parser (Src, exprFromText)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError)
import Options.Generic (Generic, ParseRecord, type (<?>)(..))
import System.IO (stderr)
import System.Exit (exitFailure)
import Text.Trifecta.Delta (Delta(..))

import qualified Control.Exception
import qualified Data.Text.Lazy.IO
import qualified Dhall.TypeCheck
import qualified Options.Generic
import qualified System.Exit
import qualified System.IO

data Mode = Default | Resolve | TypeCheck | Normalize

data Options = Options
    { explain :: Bool <?> "Explain error messages in more detail"
    } deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
    options <- Options.Generic.getRecord "Compiler for the Dhall language"

    let handle =
                Control.Exception.handle handler2
            .   Control.Exception.handle handler1
            .   Control.Exception.handle handler0
          where
            handler0 e = do
                let _ = e :: TypeError Src
                System.IO.hPutStrLn stderr ""
                if unHelpful (explain options)
                    then Control.Exception.throwIO (DetailedTypeError e)
                    else do
                        Data.Text.Lazy.IO.hPutStrLn stderr "\ESC[2mAdd the --explain flag for a more detailed explanation of this error\ESC[0m"
                        Control.Exception.throwIO e

            handler1 (Imported ps e) = do
                let _ = e :: TypeError Src
                System.IO.hPutStrLn stderr ""
                if unHelpful (explain options)
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.Lazy.IO.hPutStrLn stderr "\ESC[2mAdd the --explain flag for a more detailed explanation of this error\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handler2 e = do
                let _ = e :: SomeException
                System.IO.hPutStrLn stderr ""
                System.IO.hPrint stderr e
                System.Exit.exitFailure

    handle (do
        inText <- Data.Text.Lazy.IO.getContents

        expr <- case exprFromText (Directed "(stdin)" 0 0 0 0) inText of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr

        expr' <- load Nothing expr

        typeExpr <- case Dhall.TypeCheck.typeOf expr' of
            Left  err      -> Control.Exception.throwIO err
            Right typeExpr -> return typeExpr
        Data.Text.Lazy.IO.hPutStrLn stderr (pretty (normalize typeExpr))
        Data.Text.Lazy.IO.hPutStrLn stderr mempty
        Data.Text.Lazy.IO.putStrLn (pretty (normalize expr')) )
