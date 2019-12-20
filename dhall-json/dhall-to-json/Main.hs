module Main where

import qualified Dhall.DhallToJSON.Main
import qualified Paths_dhall_json       as Meta

main :: IO ()
main = Dhall.DhallToJSON.Main.main Meta.version
