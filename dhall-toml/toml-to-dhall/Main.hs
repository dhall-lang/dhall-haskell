module Main where

import qualified Dhall.TOML (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Dhall.TOML.someFunc
