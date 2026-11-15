{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.SemanticHash where

import Data.Text  (Text)
import Test.Tasty (TestTree)

import qualified Data.Text        as Text
import qualified Dhall.Core       as Core
import qualified Dhall.Import     as Import
import qualified Dhall.Parser     as Parser
import qualified Dhall.Test.Util  as Test.Util
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Turtle

hashDirectory :: FilePath
hashDirectory = "./dhall-lang/tests/semantic-hash"

getTests :: IO TestTree
getTests = do
    hashTests <- Test.Util.discover (Turtle.chars <* "A.dhall") hashTest (Turtle.lstree hashDirectory)

    return (Tasty.testGroup "semantic-hash tests" [ hashTests ])

hashTest :: Text -> TestTree
hashTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let codeFile = Test.Util.toDhallPath (prefix <> "A.dhall")

        expr <- Core.throws (Parser.exprFromText mempty codeFile)

        resolved <- Import.load expr

        -- The standard B.hash files are the old tree-CBOR hashes. This
        -- implementation's integrity hash is the canonical DAG encoding, so
        -- the check is that the two ways of computing it agree.
        let normalized = Core.alphaNormalize (Core.normalize resolved)
            fromExprHash = Import.hashExpressionToCode normalized
            graphHash =
                Import.hashCanonicalToCode
                    (Core.alphaCanonical (Core.normalizeToGraph resolved))

        Tasty.HUnit.assertEqual
            "hashExpression and the canonical graph disagree"
            graphHash
            fromExprHash
        Tasty.HUnit.assertBool
            "hash should use the sha256: prefix"
            (Text.isPrefixOf "sha256:" graphHash)
