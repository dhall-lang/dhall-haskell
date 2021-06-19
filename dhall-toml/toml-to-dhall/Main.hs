module Main where

import Dhall.Toml (tomlToDhallMain)

main :: IO ()
main = tomlToDhallMain
