
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Dhall.Main as Main
import           Dhall.Binary (defaultProtocolVersion)

options :: Main.Options
options = Main.Options
  { Main.mode = Main.Default False
  , Main.explain = False
  , Main.plain = False
  , Main.ascii = False
  , Main.protocolVersion = defaultProtocolVersion
  }

main :: IO ()
main = do
  Main.command options
