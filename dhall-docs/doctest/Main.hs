module Main (main) where

import System.FilePath ((</>))

import qualified GHC.IO.Encoding
import qualified System.Directory
import qualified System.Environment
import qualified System.IO
import qualified Test.DocTest

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
    args <- System.Environment.getArgs
    pwd <- System.Directory.getCurrentDirectory
    prefix <- System.Directory.makeAbsolute pwd
    let src = prefix </> "src"

    Test.DocTest.doctest $
        [ "--fast"
        ] <> args <>
        [ src
        ]
