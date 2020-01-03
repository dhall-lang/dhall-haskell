{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dhall.Test.TH where

import Dhall (FromDhall(..))
import GHC.Generics
import Test.Tasty (TestTree)

import qualified Dhall
import qualified Dhall.TH
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit

Dhall.TH.makeHaskellType "T" "./tests/th/example.dhall"

deriving instance Eq        T
deriving instance Show      T
deriving instance Generic   T
deriving instance FromDhall T

tests :: TestTree
tests = Tasty.testGroup "Template Haskell" [ makeHaskellType ]

makeHaskellType :: TestTree
makeHaskellType = Tasty.HUnit.testCase "makeHaskellType" $ do
    let interpretOptions =
            Dhall.defaultInterpretOptions
                { Dhall.singletonConstructors = Dhall.Smart
                }

    let decoder = Dhall.autoWith interpretOptions

    t0 <- Dhall.input decoder "let T = ./tests/th/example.dhall in T.A { x = True, y = [ \"ABC\" ] }"

    Tasty.HUnit.assertEqual "" t0 A{ x = True, y = [ "ABC" ] }

    t1 <- Dhall.input decoder "let T = ./tests/th/example.dhall in T.B (Some [ 1 ])"

    Tasty.HUnit.assertEqual "" t1 (B (Just [ 1 ]))

    t2 <- Dhall.input decoder "let T = ./tests/th/example.dhall in T.C"

    Tasty.HUnit.assertEqual "" t2 C
