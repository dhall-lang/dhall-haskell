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
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.TypeCheck
import qualified Data.Text.IO

-- | Implementation of the @dhall hash@ subcommand
hash :: ProtocolVersion -> IO ()
hash _protocolVersion = do
    inText <- Data.Text.IO.getContents

    parsedExpression <- case exprFromText "(stdin)" inText of
        Left  exception        -> Control.Exception.throwIO exception
        Right parsedExpression -> return parsedExpression

    let status =
            set protocolVersion _protocolVersion (Dhall.Import.emptyStatus ".")

    resolvedExpression <- State.evalStateT (Dhall.Import.loadWith parsedExpression) status

    case Dhall.TypeCheck.typeOf resolvedExpression of
        Left  exception -> Control.Exception.throwIO exception
        Right _         -> return ()

    let normalizedExpression =
            Dhall.Core.alphaNormalize (Dhall.Core.normalize resolvedExpression)

    Data.Text.IO.putStrLn
        (hashExpressionToCode _protocolVersion normalizedExpression)
