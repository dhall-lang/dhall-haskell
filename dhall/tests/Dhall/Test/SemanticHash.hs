{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.SemanticHash where

import Data.Monoid((<>))
import Data.Text (Text)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath)

import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as Text.IO
import qualified Dhall.Binary                          as Binary
import qualified Dhall.Core                            as Core
import qualified Dhall.Import                          as Import
import qualified Dhall.Parser                          as Parser
import qualified Dhall.Test.Util                       as Test.Util
import qualified Test.Tasty                            as Tasty
import qualified Test.Tasty.HUnit                      as Tasty.HUnit
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
        let hashFile = Test.Util.toDhallPath (prefix <> "B.hash")

        expr <- Core.throws (Parser.exprFromText mempty codeFile)

        resolved <- Import.load expr

        let normalized = Core.alphaNormalize (Core.normalize resolved)

        let actualHash = Import.hashExpressionToCode Binary.defaultStandardVersion normalized

        expectedHash <- Text.stripEnd <$> Text.IO.readFile (Text.unpack hashFile)

        let message = "The hash did not match the expected hash."

        Tasty.HUnit.assertEqual message actualHash expectedHash
