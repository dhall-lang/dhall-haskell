{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall hash@ subcommand

module Dhall.Hash
    ( -- * Hash
      hash
    ) where

import Dhall.Binary (ProtocolVersion)
import Dhall.Parser (exprFromText)
import Dhall.Import (hashExpressionToCode, protocolVersion)
import Lens.Family (set)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Exception
import qualified Dhall.Import
import qualified Dhall.TypeCheck
import qualified Data.Text.IO

-- | Implementation of the @dhall hash@ subcommand
hash :: ProtocolVersion -> IO ()
hash _protocolVersion = do
    inText <- Data.Text.IO.getContents

    expr <- case exprFromText "(stdin)" inText of
        Left  err  -> Control.Exception.throwIO err
        Right expr -> return expr

    let status =
            set protocolVersion _protocolVersion (Dhall.Import.emptyStatus ".")

    expr' <- State.evalStateT (Dhall.Import.loadWith expr) status

    _ <- case Dhall.TypeCheck.typeOf expr' of
        Left  err -> Control.Exception.throwIO err
        Right _   -> return ()

    Data.Text.IO.putStrLn (hashExpressionToCode _protocolVersion expr')
