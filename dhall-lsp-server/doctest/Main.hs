module Main where

import Data.Monoid ((<>))
import System.FilePath ((</>))

import qualified GHC.IO.Encoding
import qualified System.Directory
import qualified System.IO
import qualified Test.DocTest

main :: IO ()
main = do

    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
    pwd    <- System.Directory.getCurrentDirectory
    prefix <- System.Directory.makeAbsolute pwd

    Test.DocTest.doctest
        [ "--fast"
        , "-XOverloadedStrings"
        , "-XRecordWildCards"
        , "-i" <> (prefix </> "src")
        , prefix </> "src/Dhall/LSP/Backend/Diagnostics.hs"
        ]
