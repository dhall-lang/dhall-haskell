{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Criterion.Main (defaultMain, bgroup, bench, whnf, nfIO)
import Data.Map (Map, foldrWithKey, singleton, unions)

import System.Directory

import qualified Criterion.Main as Criterion
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Dhall.Parser as Dhall

type PreludeFiles = Map FilePath TL.Text

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
        loadFile path = singleton path <$> TLIO.readFile path

benchParser :: PreludeFiles -> Criterion.Benchmark
benchParser =
      bgroup "exprFromText"
    . foldrWithKey (\name expr -> (benchExprFromText name expr :)) []

benchExprFromText :: String -> TL.Text -> Criterion.Benchmark
benchExprFromText name expr =
    bench name $ whnf (Dhall.exprFromText "(input)") expr

main :: IO ()
main = do
    prelude <- loadPreludeFiles
    issue108 <- TLIO.readFile "benchmark/examples/issue108.dhall"
    defaultMain
        [ benchParser prelude
        , bgroup "Issue #108" $
            [ benchExprFromText "108" issue108 ]
        ]
