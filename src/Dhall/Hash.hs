{-# LANGUAGE OverloadedStrings #-}

module Dhall.Hash ( hash ) where

import Dhall.Parser (exprFromText)
import Dhall.Import (hashExpressionToCode, load)

import qualified Control.Exception
import qualified Dhall.TypeCheck
import qualified Data.Text.IO

hash :: IO ()
hash = do
        inText <- Data.Text.IO.getContents

        expr <- case exprFromText "(stdin)" inText of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr

        expr' <- load expr

        _ <- case Dhall.TypeCheck.typeOf expr' of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()

        Data.Text.IO.putStrLn (hashExpressionToCode expr')
