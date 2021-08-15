module Main where

import System.FilePath ((</>))

import qualified System.Directory
import qualified Test.DocTest

main :: IO ()
main = do
    pwd <- System.Directory.getCurrentDirectory
    prefix <- System.Directory.makeAbsolute pwd
    let src = prefix </> "src"
    Test.DocTest.doctest [ "--fast",  "-i" <> src, src ]

