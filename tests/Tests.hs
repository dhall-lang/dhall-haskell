module Main where

import Normalization (normalizationTests)
import Test.Tasty

main :: IO ()
main = defaultMain (testGroup "Dhall Tests" [ normalizationTests ])
