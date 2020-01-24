{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Data.Map (Map, foldrWithKey, singleton, unions)
import Data.Monoid ((<>))
import Data.Void (Void)
import Gauge (defaultMain, bgroup, bench, nf, whnf, nfIO)

import System.Directory

import qualified Codec.Serialise
import qualified Gauge
import qualified Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Dhall.Binary
import qualified Dhall.Core as Dhall
import qualified Dhall.Parser as Dhall

benchExprFromText :: String -> T.Text -> Gauge.Benchmark
benchExprFromText name expr =
    bench name $ nf (Dhall.exprFromText "(input)") expr

main :: IO ()
main = do
    cpkgExample <- TIO.readFile "benchmark/normalize/cpkg.dhall"
    defaultMain
        [ benchExprFromText "CPkg/Text" cpkgExample ]
