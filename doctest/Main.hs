module Main where

import qualified Test.DocTest

main :: IO ()
main =
    Test.DocTest.doctest
        [ "-isrc"
        , "src/Dhall.hs"
        , "src/Dhall/Import.hs"
        , "src/Dhall/Tutorial.hs"
        ]
