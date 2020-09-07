module Main where

import qualified Dhall.DhallToYaml.Main
import qualified Dhall.JSON.Yaml
import qualified Paths_dhall_json       as Meta

main :: IO ()
main = Dhall.DhallToYaml.Main.main Meta.version Dhall.JSON.Yaml.dhallToYaml
