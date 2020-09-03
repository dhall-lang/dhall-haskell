module Main where

import qualified Dhall.DhallToYaml.Main
import qualified Dhall.Yaml
import qualified Paths_dhall_yaml       as Meta

main :: IO ()
main = Dhall.DhallToYaml.Main.main
           Meta.version
           (flip Dhall.Yaml.dhallToYaml Dhall.Yaml.defaultNewManager)
