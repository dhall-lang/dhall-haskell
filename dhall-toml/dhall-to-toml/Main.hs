module Main where

import qualified Dhall.Toml (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Dhall.Toml.someFunc
