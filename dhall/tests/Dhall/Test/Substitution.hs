{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Substitution where

import Control.Exception (throwIO)
import Data.Void (Void)
import Dhall.Core (Expr(BoolLit, Var))
import Dhall.Src (Src)

import qualified Data.Either.Validation
import qualified Dhall
import qualified Dhall.Map
import qualified Lens.Family   as Lens

data Result = Failure Integer | Success String
    deriving (Eq, Dhall.Generic, Show)

instance Dhall.FromDhall Result

substituteResult :: FilePath -> IO Result
substituteResult fp = do
    rt <- resultType
    let evaluateSettings = Lens.over Dhall.substitutions (Dhall.Map.insert "Result" rt) Dhall.defaultEvaluateSettings
    Dhall.inputFileWithSettings evaluateSettings resultDecoder fp

resultDecoder :: Dhall.Decoder Result
resultDecoder = Dhall.auto

resultType :: IO (Expr Src Void)
resultType = case Dhall.expected resultDecoder of
    Data.Either.Validation.Failure e -> throwIO e
    Data.Either.Validation.Success x -> return x

substituteFoo :: FilePath -> IO Bool
substituteFoo fp = let
    evaluateSettings = Lens.set Dhall.substitutions (Dhall.Map.fromList [("Foo", Var "Bar"), ("Bar", BoolLit True)]) Dhall.defaultEvaluateSettings
    in Dhall.inputFileWithSettings evaluateSettings Dhall.auto fp
