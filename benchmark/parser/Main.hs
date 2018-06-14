{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Criterion.Main (defaultMain, bgroup, bench, whnf, nfIO)
import Data.Map (Map, foldrWithKey, singleton, unions)

import System.Directory

import qualified Criterion.Main as Criterion
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Dhall.Parser as Dhall

type PreludeFiles = Map FilePath T.Text

loadPreludeFiles :: IO PreludeFiles
loadPreludeFiles = loadDirectory "Prelude"
    where
        loadDirectory :: FilePath -> IO PreludeFiles
        loadDirectory dir =
            withCurrentDirectory dir $ do
                files <- getCurrentDirectory >>= listDirectory
                results <- forM files $ \file -> do
                    file' <- makeAbsolute file
                    doesExist <- doesFileExist file'
                    if doesExist
                       then loadFile file'
                       else loadDirectory file'
                pure $ unions results

        loadFile :: FilePath -> IO PreludeFiles
        loadFile path = singleton path <$> TIO.readFile path

benchParser :: PreludeFiles -> Criterion.Benchmark
benchParser =
      bgroup "exprFromText"
    . foldrWithKey (\name expr -> (benchExprFromText name expr :)) []

benchExprFromText :: String -> T.Text -> Criterion.Benchmark
benchExprFromText name expr =
    bench name $ whnf (Dhall.exprFromText "(input)") expr

main :: IO ()
main = do
    prelude <- loadPreludeFiles
    issue108 <- TIO.readFile "benchmark/examples/issue108.dhall"
    defaultMain
        [ benchParser prelude
        , bgroup "Issue #108" $
            [ benchExprFromText "108" issue108 ]
        ]
