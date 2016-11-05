{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Control.Exception (Exception, throwIO)
import Data.Monoid (mempty)
import Data.Traversable
import Dhall.Core (pretty, normalize)
import Dhall.Import (load)
import Dhall.Parser (exprFromText)
import Dhall.TypeCheck (DetailedTypeError(..))
import Options.Generic (Generic, ParseRecord, type (<?>)(..))
import System.IO (stderr)
import System.Exit (exitFailure)
import Text.Trifecta.Delta (Delta(..))

import qualified Data.Text.Lazy.IO
import qualified Dhall.TypeCheck
import qualified Options.Generic

data Mode = Default | Resolve | TypeCheck | Normalize

data Options = Options
    { explain :: Bool <?> "Explain error messages in more detail"
    } deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
    options <- Options.Generic.getRecord "Compiler for the Dhall language"

    inText <- Data.Text.Lazy.IO.getContents

    expr <- case exprFromText (Directed "(stdin)" 0 0 0 0) inText of
        Left  err  -> throwIO err
        Right expr -> return expr

    expr' <- load Nothing expr

    typeExpr <- case Dhall.TypeCheck.typeOf expr' of
        Left  err  -> do
            if unHelpful (explain options)
                then throwIO (DetailedTypeError err)
                else throwIO err
        Right typeExpr -> do
            return typeExpr
    Data.Text.Lazy.IO.hPutStrLn stderr (pretty (normalize typeExpr))
    Data.Text.Lazy.IO.hPutStrLn stderr mempty
    Data.Text.Lazy.IO.putStrLn (pretty (normalize expr'))
