
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Dhall.Main as Main
import           Dhall.Binary (defaultStandardVersion)

options :: Main.Options
options = Main.Options
  { Main.mode = Main.Default Nothing False False
  , Main.explain = False
  , Main.plain = False
  , Main.ascii = False
  , Main.standardVersion = defaultStandardVersion
  }

main :: IO ()
main = do
  Main.command options
