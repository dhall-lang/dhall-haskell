{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall hash@ subcommand

module Dhall.Hash
    ( -- * Hash
      hash
    ) where

import Dhall.Import (hashExpressionToCode)
import Dhall.Util (Censor, Input(..))

import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Exception
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.TypeCheck
import qualified Dhall.Util
import qualified Data.Text.IO

-- | Implementation of the @dhall hash@ subcommand
hash :: Censor -> IO ()
hash censor = do
    parsedExpression <- Dhall.Util.getExpression censor StandardInput

    let status = Dhall.Import.emptyStatus "."

    resolvedExpression <- State.evalStateT (Dhall.Import.loadWith parsedExpression) status

    case Dhall.TypeCheck.typeOf resolvedExpression of
        Left  exception -> Control.Exception.throwIO exception
        Right _         -> return ()

    let normalizedExpression =
            Dhall.Core.alphaNormalize (Dhall.Core.normalize resolvedExpression)

    Data.Text.IO.putStrLn (hashExpressionToCode normalizedExpression)
