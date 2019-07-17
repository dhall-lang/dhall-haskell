module Main where

import Data.Monoid ((<>))
import System.FilePath ((</>))

import Build_doctests (flags, pkgs, module_sources)

import qualified GHC.IO.Encoding
import qualified System.Directory
import qualified System.IO
import qualified Test.Mockery.Directory
import qualified Test.DocTest

main :: IO ()
main = do
   
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8 
    pwd    <- System.Directory.getCurrentDirectory
    prefix <- System.Directory.makeAbsolute pwd

    Test.Mockery.Directory.inTempDirectory $ do
        writeFile "makeBools.dhall" "λ(n : Bool) → [ n && True, n && False, n || True, n || False ]"
        writeFile "bool1" "True"
        writeFile "bool2" "False"
        writeFile "both" "./bool1 && ./bool2"
        writeFile "file2" "./file1"
        writeFile "file1" "./file2"

        let cpp_flags =
              [ "-DWITH_CPP"
              ]
        Test.DocTest.doctest $
            cpp_flags ++ flags ++ pkgs ++ module_sources
