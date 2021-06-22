module Main where

import Dhall.Toml (tomlToDhall)

main :: IO ()
main = tomlToDhall
