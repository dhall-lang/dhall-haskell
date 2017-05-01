{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings#-}
module Normalization (normalizationTests) where

import           Dhall.Core
import           Dhall.TypeCheck
import           Test.Tasty
import           Test.Tasty.HUnit

normalizationTests :: TestTree
normalizationTests = testGroup "normalization" [ constantFolding
                                               , fusion
                                               ]

constantFolding :: TestTree
constantFolding = testGroup "folding of constants" [ naturalPlus, optionalFold, optionalBuild ]

naturalPlus :: TestTree
naturalPlus = testCase "natural plus" $ do
  isNormalized e @?= False
  normalize' e @?= NaturalLit 3
  where e = NaturalPlus (NaturalLit 1) (NaturalLit 2)

optionalFold :: TestTree
optionalFold = testGroup "Optional/fold" [ just, nothing ]
  where test label inp out = testCase label $ do
             isNormalized (e inp) @?= False
             normalize' (e inp) @?= out
        e inp = (OptionalFold `App` Text `App` OptionalLit Text inp `App`
                                             Natural `App` (Lam "j" Text (NaturalLit 1)) `App`
                                                 (NaturalLit 2))
        just = test "just" [TextLit "foo"] (NaturalLit 1)
        nothing = test "nothing" [] (NaturalLit 2)

optionalBuild :: TestTree
optionalBuild = testGroup "Optional/build" [ optionalBuild1
                                           , optionalBuildShadowing
                                           , optionalBuildIrreducible
                                           ]

optionalBuild1 :: TestTree
optionalBuild1 = testCase "reducible" $ do
  isNormalized e @?= False
  normalize' e @?= OptionalLit Natural [NaturalLit 1]
  where e = OptionalBuild `App` Natural `App`
              (Lam "optional" (Const Type)
                  (Lam "just" (Pi "_" Natural "optional")
                      (Lam "nothing" "optional"
                          (App "just" (NaturalLit 1)))))

optionalBuildShadowing :: TestTree
optionalBuildShadowing = testCase "handles shadowing" $ do
  isNormalized e @?= False
  normalize' e @?= OptionalLit Integer [IntegerLit 1]
  where e = OptionalBuild `App` Integer `App`
                (Lam "optional" (Const Type)
                    (Lam "x" (Pi "_" Integer "optional")
                        (Lam "x" "optional"
                            (App (Var (V "x" 1)) (IntegerLit 1)))))

optionalBuildIrreducible :: TestTree
optionalBuildIrreducible = testCase "irreducible" $ do
  isNormalized e @?= True
  normalize' e @?= e
  where e = Lam "id" (Pi "a" (Const Type) (Pi "_" "a" "a"))
                (OptionalBuild `App` Bool `App`
                    (Lam "optional" (Const Type)
                        (Lam "just" (Pi "_" Bool "optional")
                            (Lam "nothing" "optional"
                                ("id" `App` "optional" `App` "just" `App` BoolLit True)))))

normalize' :: Expr () X -> Expr () X
normalize' = normalize

fusion :: TestTree
fusion = testGroup "Optional build/fold fusion" [ fuseOptionalBF
                                                , fuseOptionalFB
                                                ]

fuseOptionalBF :: TestTree
fuseOptionalBF = testCase "fold . build" $ do
  isNormalized (test j) @?= False
  normalize' (test j) @?= NaturalLit 42
  isNormalized (test n) @?= False
  normalize' (test n) @?= NaturalLit 2
  where test e = OptionalFold `App` Text `App` (opt e) `App` Natural `App` (Lam "j" Text (NaturalLit 42)) `App`
                      (NaturalLit 2)
        opt e = OptionalBuild `App` Text `App`
                    (Lam "optional" (Const Type)
                        (Lam "just" (Pi "_" Text "optional")
                            (Lam "nothing" "optional"
                                e)))
        j = (App "just" (TextLit "foo"))
        n = "nothing"

fuseOptionalFB :: TestTree
fuseOptionalFB = testCase "build . fold" $ do
  isNormalized test @?= False
  normalize' test @?= OptionalLit Natural [NaturalLit 42]
  where test = OptionalBuild `App` Natural `App`
                   (Lam "optional" (Const Type)
                       (Lam "just" (Pi "_" Natural "optional")
                           (Lam "nothing" "optional" (App "just" fold))))
        fold = OptionalFold `App` Text `App` (OptionalLit Text [TextLit "foo"]) `App` Natural `App` (Lam "just" Text (NaturalLit 42)) `App` (NaturalLit 2)
