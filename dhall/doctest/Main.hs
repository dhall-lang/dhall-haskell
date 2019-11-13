module Main where

import Data.Monoid ((<>))
import System.FilePath ((</>))

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

        Test.DocTest.doctest
            [ "-DWITH_HTTP"
            , "--fast"
            , prefix </> "ghc-src"

            -- Unfortunately we cannot target the entire @src@ directory.
            -- The reason is that src/Dhall/Version.hs depends on
            -- the generated Paths_dhall module which is "out-of-scope"
            -- when running the testsuite with cabal v1-test.
            -- Instead, we target a selection of modules whose combined module
            -- dependency tree covers all modules that contain doctests.

            -- , prefix </> "src"
            , "-i" <> (prefix </> "src")
            , prefix </> "src/Dhall/Tags.hs"
            , prefix </> "src/Dhall/Tutorial.hs"
            ]
