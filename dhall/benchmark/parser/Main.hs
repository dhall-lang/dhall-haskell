{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Data.Map (Map, foldrWithKey, singleton, unions)
import Data.Monoid ((<>))
import Gauge (defaultMain, bgroup, bench, whnf, nfIO)

import System.Directory

import qualified Codec.Serialise
import qualified Gauge
import qualified Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Dhall.Binary
import qualified Dhall.Parser as Dhall

#if MIN_VERSION_directory(1,2,3)
#else
import Control.Exception (bracket)

withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

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

benchParser :: PreludeFiles -> Gauge.Benchmark
benchParser =
      bgroup "exprFromText"
    . foldrWithKey (\name expr -> (benchExprFromText name expr :)) []

benchExprFromText :: String -> T.Text -> Gauge.Benchmark
benchExprFromText name expr =
    bench name $ whnf (Dhall.exprFromText "(input)") expr

benchExprFromBytes
    :: String -> Data.ByteString.Lazy.ByteString -> Gauge.Benchmark
benchExprFromBytes name bytes = bench name (whnf f bytes)
  where
    f bytes = do
        term <- case Codec.Serialise.deserialiseOrFail bytes of
            Left  _    -> Nothing
            Right term -> return term
        case Dhall.Binary.decodeExpression term of
            Left  _          -> Nothing
            Right expression -> return expression

main :: IO ()
main = do
    prelude <- loadPreludeFiles
    issue108Text  <- TIO.readFile "benchmark/examples/issue108.dhall"
    issue108Bytes <- Data.ByteString.Lazy.readFile "benchmark/examples/issue108.dhall.bin"
    defaultMain
        [ bgroup "Issue #108"
            [ benchExprFromText  "Text"   issue108Text
            , benchExprFromBytes "Binary" issue108Bytes
            ]
        , benchExprFromText "Long variable names" (T.replicate 1000000 "x")
        , benchExprFromText "Large number of function arguments" (T.replicate 10000 "x ")
        , benchExprFromText "Long double-quoted strings" ("\"" <> T.replicate 1000000 "x" <> "\"")
        , benchExprFromText "Long single-quoted strings" ("''" <> T.replicate 1000000 "x" <> "''")
        , benchExprFromText "Whitespace" (T.replicate 1000000 " " <> "x")
        , benchExprFromText "Line comment" ("x -- " <> T.replicate 1000000 " ")
        , benchExprFromText "Block comment" ("x {- " <> T.replicate 1000000 " " <> "-}")
        , benchExprFromText "Deeply nested parentheses" "((((((((((((((((x))))))))))))))))"
        , benchParser prelude
        ]
