module Main where

import Data.Monoid ((<>))
import System.FilePath ((</>))

import qualified System.Directory
import qualified Test.Mockery.Directory
import qualified Test.DocTest

main :: IO ()
main = do
    pwd    <- System.Directory.getCurrentDirectory
    prefix <- System.Directory.makeAbsolute pwd

    Test.Mockery.Directory.inTempDirectory $ do
        writeFile "makeBools" "λ(n : Bool) → [ n && True, n && False, n || True, n || False ]"
        writeFile "bool1" "True"
        writeFile "bool2" "False"
        writeFile "both" "./bool1 && ./bool2"
        writeFile "file2" "./file1"
        writeFile "file1" "./file2"

        Test.DocTest.doctest
            [ "--fast"
            , "-i" <> (prefix </> "src")
            , prefix </> "src/Dhall.hs"
            , prefix </> "src/Dhall/Import.hs"
            , prefix </> "src/Dhall/Tutorial.hs"
            ]
