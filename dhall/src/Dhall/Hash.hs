{-# LANGUAGE OverloadedStrings #-}
{-# language RecordWildCards   #-}

-- | This module contains the implementation of the @dhall hash@ subcommand

module Dhall.Hash
    ( -- * Hash
      hash
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Dhall.Binary (StandardVersion)
import Dhall.Parser (exprFromText)
import Dhall.Import (hashExpressionToCode, standardVersion, LoadedExpr(..))
import Lens.Family (set)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Exception
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.TypeCheck
import qualified Data.Text.IO

-- | Implementation of the @dhall hash@ subcommand
hash :: StandardVersion -> IO ()
hash _standardVersion = do
    inText <- Data.Text.IO.getContents

    parsedExpression <- case exprFromText "(stdin)" inText of
        Left  exception        -> Control.Exception.throwIO exception
        Right parsedExpression -> return parsedExpression

    let status =
            set standardVersion _standardVersion Dhall.Import.emptyStatus

    let stack = Dhall.Import.rootImport "." :| []

    LoadedExpr {..} <-
        State.evalStateT (Dhall.Import.loadExpr stack parsedExpression) status

    case Dhall.TypeCheck.typeOf loadedExpr of
        Left  exception -> Control.Exception.throwIO exception
        Right _         -> return ()

    let normalizedExpression =
            Dhall.Core.alphaNormalize (Dhall.Core.normalize loadedExpr)

    Data.Text.IO.putStrLn
        (hashExpressionToCode _standardVersion normalizedExpression)
