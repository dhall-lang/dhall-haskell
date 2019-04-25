{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Normalization where

import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall.Parser

import Dhall.Lint
import Dhall.Eval
import Dhall.Elaboration
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup "normalization"
        [ tutorialExamples
        , preludeExamples
        , unitTests
        , alphaNormalizationTests
        , simplifications
        , constantFolding
        , conversions
        , shouldNormalize
            "Optional build/fold fusion"
            "success/simple/optionalBuildFold"
        , shouldNormalize
            "a remote-systems.conf builder"
            "success/remoteSystems"
        , shouldNormalize
            "multi-line strings correctly"
            "success/simple/multiLine"
        , shouldNormalize
            "the // operator and sort the fields"
             "success/simple/sortOperator"
        , shouldNormalize
            "enums"
            "success/simple/enum"
        ]

tutorialExamples :: TestTree
tutorialExamples =
    testGroup "Tutorial examples"
        [ shouldNormalize "⩓" "./success/haskell-tutorial/combineTypes/0"
        , shouldNormalize "//\\\\" "./success/haskell-tutorial/combineTypes/1"
        , shouldNormalize "//" "./success/haskell-tutorial/prefer/0"
        , shouldNormalize "projection" "./success/haskell-tutorial/projection/0"
        , shouldNormalize "access record" "./success/haskell-tutorial/access/0"
        , shouldNormalize "access union" "./success/haskell-tutorial/access/1"
        ]

preludeExamples :: TestTree
preludeExamples =
    testGroup "Prelude examples"
        [ shouldNormalize "Bool/and" "./success/prelude/Bool/and/0"
        , shouldNormalize "Bool/and" "./success/prelude/Bool/and/1"
        , shouldNormalize "Bool/build" "./success/prelude/Bool/build/0"
        , shouldNormalize "Bool/build" "./success/prelude/Bool/build/1"
        , shouldNormalize "Bool/even" "./success/prelude/Bool/even/0"
        , shouldNormalize "Bool/even" "./success/prelude/Bool/even/1"
        , shouldNormalize "Bool/even" "./success/prelude/Bool/even/2"
        , shouldNormalize "Bool/even" "./success/prelude/Bool/even/3"
        , shouldNormalize "Bool/fold" "./success/prelude/Bool/fold/0"
        , shouldNormalize "Bool/fold" "./success/prelude/Bool/fold/1"
        , shouldNormalize "Bool/not" "./success/prelude/Bool/not/0"
        , shouldNormalize "Bool/not" "./success/prelude/Bool/not/1"
        , shouldNormalize "Bool/odd" "./success/prelude/Bool/odd/0"
        , shouldNormalize "Bool/odd" "./success/prelude/Bool/odd/1"
        , shouldNormalize "Bool/odd" "./success/prelude/Bool/odd/2"
        , shouldNormalize "Bool/odd" "./success/prelude/Bool/odd/3"
        , shouldNormalize "Bool/or" "./success/prelude/Bool/or/0"
        , shouldNormalize "Bool/or" "./success/prelude/Bool/or/1"
        , shouldNormalize "Bool/show" "./success/prelude/Bool/show/0"
        , shouldNormalize "Bool/show" "./success/prelude/Bool/show/1"
        , shouldNormalize "Double/show" "./success/prelude/Double/show/0"
        , shouldNormalize "Double/show" "./success/prelude/Double/show/1"
        , shouldNormalize "Integer/show" "./success/prelude/Integer/show/0"
        , shouldNormalize "Integer/show" "./success/prelude/Integer/show/1"
        , shouldNormalize "Integer/toDouble" "./success/prelude/Integer/toDouble/0"
        , shouldNormalize "Integer/toDouble" "./success/prelude/Integer/toDouble/1"
        , shouldNormalize "List/all" "./success/prelude/List/all/0"
        , shouldNormalize "List/all" "./success/prelude/List/all/1"
        , shouldNormalize "List/any" "./success/prelude/List/any/0"
        , shouldNormalize "List/any" "./success/prelude/List/any/1"
        , shouldNormalize "List/build" "./success/prelude/List/build/0"
        , shouldNormalize "List/build" "./success/prelude/List/build/1"
        , shouldNormalize "List/concat" "./success/prelude/List/concat/0"
        , shouldNormalize "List/concat" "./success/prelude/List/concat/1"
        , shouldNormalize "List/concatMap" "./success/prelude/List/concatMap/0"
        , shouldNormalize "List/concatMap" "./success/prelude/List/concatMap/1"
        , shouldNormalize "List/filter" "./success/prelude/List/filter/0"
        , shouldNormalize "List/filter" "./success/prelude/List/filter/1"
        , shouldNormalize "List/fold" "./success/prelude/List/fold/0"
        , shouldNormalize "List/fold" "./success/prelude/List/fold/1"
        , shouldNormalize "List/fold" "./success/prelude/List/fold/2"
        , shouldNormalize "List/generate" "./success/prelude/List/generate/0"
        , shouldNormalize "List/generate" "./success/prelude/List/generate/1"
        , shouldNormalize "List/head" "./success/prelude/List/head/0"
        , shouldNormalize "List/head" "./success/prelude/List/head/1"
        , shouldNormalize "List/indexed" "./success/prelude/List/indexed/0"
        , shouldNormalize "List/indexed" "./success/prelude/List/indexed/1"
        , shouldNormalize "List/iterate" "./success/prelude/List/iterate/0"
        , shouldNormalize "List/iterate" "./success/prelude/List/iterate/1"
        , shouldNormalize "List/last" "./success/prelude/List/last/0"
        , shouldNormalize "List/last" "./success/prelude/List/last/1"
        , shouldNormalize "List/length" "./success/prelude/List/length/0"
        , shouldNormalize "List/length" "./success/prelude/List/length/1"
        , shouldNormalize "List/map" "./success/prelude/List/map/0"
        , shouldNormalize "List/map" "./success/prelude/List/map/1"
        , shouldNormalize "List/null" "./success/prelude/List/null/0"
        , shouldNormalize "List/null" "./success/prelude/List/null/1"
        , shouldNormalize "List/replicate" "./success/prelude/List/replicate/0"
        , shouldNormalize "List/replicate" "./success/prelude/List/replicate/1"
        , shouldNormalize "List/reverse" "./success/prelude/List/reverse/0"
        , shouldNormalize "List/reverse" "./success/prelude/List/reverse/1"
        , shouldNormalize "List/shifted" "./success/prelude/List/shifted/0"
        , shouldNormalize "List/shifted" "./success/prelude/List/shifted/1"
        , shouldNormalize "List/unzip" "./success/prelude/List/unzip/0"
        , shouldNormalize "List/unzip" "./success/prelude/List/unzip/1"
        , shouldNormalize "Natural/build" "./success/prelude/Natural/build/0"
        , shouldNormalize "Natural/build" "./success/prelude/Natural/build/1"
        , shouldNormalize "Natural/enumerate" "./success/prelude/Natural/enumerate/0"
        , shouldNormalize "Natural/enumerate" "./success/prelude/Natural/enumerate/1"
        , shouldNormalize "Natural/even" "./success/prelude/Natural/even/0"
        , shouldNormalize "Natural/even" "./success/prelude/Natural/even/1"
        , shouldNormalize "Natural/fold" "./success/prelude/Natural/fold/0"
        , shouldNormalize "Natural/fold" "./success/prelude/Natural/fold/1"
        , shouldNormalize "Natural/fold" "./success/prelude/Natural/fold/2"
        , shouldNormalize "Natural/isZero" "./success/prelude/Natural/isZero/0"
        , shouldNormalize "Natural/isZero" "./success/prelude/Natural/isZero/1"
        , shouldNormalize "Natural/odd" "./success/prelude/Natural/odd/0"
        , shouldNormalize "Natural/odd" "./success/prelude/Natural/odd/1"
        , shouldNormalize "Natural/product" "./success/prelude/Natural/product/0"
        , shouldNormalize "Natural/product" "./success/prelude/Natural/product/1"
        , shouldNormalize "Natural/show" "./success/prelude/Natural/show/0"
        , shouldNormalize "Natural/show" "./success/prelude/Natural/show/1"
        , shouldNormalize "Natural/sum" "./success/prelude/Natural/sum/0"
        , shouldNormalize "Natural/sum" "./success/prelude/Natural/sum/1"
        , shouldNormalize "Natural/toDouble" "./success/prelude/Natural/toDouble/0"
        , shouldNormalize "Natural/toDouble" "./success/prelude/Natural/toDouble/1"
        , shouldNormalize "Natural/toInteger" "./success/prelude/Natural/toInteger/0"
        , shouldNormalize "Natural/toInteger" "./success/prelude/Natural/toInteger/1"
        , shouldNormalize "Optional/all" "./success/prelude/Optional/all/0"
        , shouldNormalize "Optional/all" "./success/prelude/Optional/all/1"
        , shouldNormalize "Optional/any" "./success/prelude/Optional/any/0"
        , shouldNormalize "Optional/any" "./success/prelude/Optional/any/1"
        , shouldNormalize "Optional/build" "./success/prelude/Optional/build/0"
        , shouldNormalize "Optional/build" "./success/prelude/Optional/build/1"
        , shouldNormalize "Optional/concat" "./success/prelude/Optional/concat/0"
        , shouldNormalize "Optional/concat" "./success/prelude/Optional/concat/1"
        , shouldNormalize "Optional/concat" "./success/prelude/Optional/concat/2"
        , shouldNormalize "Optional/filter" "./success/prelude/Optional/filter/0"
        , shouldNormalize "Optional/filter" "./success/prelude/Optional/filter/1"
        , shouldNormalize "Optional/fold" "./success/prelude/Optional/fold/0"
        , shouldNormalize "Optional/fold" "./success/prelude/Optional/fold/1"
        , shouldNormalize "Optional/head" "./success/prelude/Optional/head/0"
        , shouldNormalize "Optional/head" "./success/prelude/Optional/head/1"
        , shouldNormalize "Optional/head" "./success/prelude/Optional/head/2"
        , shouldNormalize "Optional/last" "./success/prelude/Optional/last/0"
        , shouldNormalize "Optional/last" "./success/prelude/Optional/last/1"
        , shouldNormalize "Optional/last" "./success/prelude/Optional/last/2"
        , shouldNormalize "Optional/length" "./success/prelude/Optional/length/0"
        , shouldNormalize "Optional/length" "./success/prelude/Optional/length/1"
        , shouldNormalize "Optional/map" "./success/prelude/Optional/map/0"
        , shouldNormalize "Optional/map" "./success/prelude/Optional/map/1"
        , shouldNormalize "Optional/null" "./success/prelude/Optional/null/0"
        , shouldNormalize "Optional/null" "./success/prelude/Optional/null/1"
        , shouldNormalize "Optional/toList" "./success/prelude/Optional/toList/0"
        , shouldNormalize "Optional/toList" "./success/prelude/Optional/toList/1"
        , shouldNormalize "Optional/unzip" "./success/prelude/Optional/unzip/0"
        , shouldNormalize "Optional/unzip" "./success/prelude/Optional/unzip/1"
        , shouldNormalize "Text/concat" "./success/prelude/Text/concat/0"
        , shouldNormalize "Text/concat" "./success/prelude/Text/concat/1"
        , shouldNormalize "Text/concatMap" "./success/prelude/Text/concatMap/0"
        , shouldNormalize "Text/concatMap" "./success/prelude/Text/concatMap/1"
        , shouldNormalize "Text/concatMapSep" "./success/prelude/Text/concatMapSep/0"
        , shouldNormalize "Text/concatMapSep" "./success/prelude/Text/concatMapSep/1"
        , shouldNormalize "Text/concatSep" "./success/prelude/Text/concatSep/0"
        , shouldNormalize "Text/concatSep" "./success/prelude/Text/concatSep/1"
        , shouldNormalize "Text/show" "./success/prelude/Text/show/0"
        , shouldNormalize "Text/show" "./success/prelude/Text/show/1"
        ]

unitTests :: TestTree
unitTests =
    testGroup "Unit tests"
        [ shouldOnlyNormalize "Bool"
        , shouldOnlyNormalize "Double"
        , shouldOnlyNormalize "DoubleLiteral"
        , shouldOnlyNormalize "DoubleShow"
        , shouldOnlyNormalize "DoubleShowValue"
        , shouldOnlyNormalize "FunctionApplicationCapture"
        , shouldOnlyNormalize "FunctionApplicationNoSubstitute"
        , shouldOnlyNormalize "FunctionApplicationNormalizeArguments"
        , shouldOnlyNormalize "FunctionApplicationSubstitute"
        , shouldOnlyNormalize "FunctionNormalizeArguments"
        , shouldOnlyNormalize "FunctionTypeNormalizeArguments"
        , shouldOnlyNormalize "IfAlternativesIdentical"
        , shouldOnlyNormalize "IfFalse"
        , shouldOnlyNormalize "IfNormalizePredicateAndBranches"
        , shouldOnlyNormalize "IfTrivial"
        , shouldOnlyNormalize "IfTrue"
        , shouldOnlyNormalize "Integer"
        , shouldOnlyNormalize "IntegerNegative"
        , shouldOnlyNormalize "IntegerPositive"
        , shouldOnlyNormalize "IntegerShow"
        , shouldOnlyNormalize "IntegerShow-12"
        , shouldOnlyNormalize "IntegerShow12"
        , shouldOnlyNormalize "IntegerToDouble"
        , shouldOnlyNormalize "IntegerToDouble-12"
        , shouldOnlyNormalize "IntegerToDouble12"
        , shouldOnlyNormalize "Kind"
        , shouldOnlyNormalize "Let"
        , shouldOnlyNormalize "LetWithType"
        , shouldOnlyNormalize "List"
        , shouldOnlyNormalize "ListBuild"
        , shouldOnlyNormalize "ListBuildFoldFusion"
        , shouldOnlyNormalize "ListBuildImplementation"
        , shouldOnlyNormalize "ListFold"
        , shouldOnlyNormalize "ListFoldEmpty"
        , shouldOnlyNormalize "ListFoldOne"
        , shouldOnlyNormalize "ListHead"
        , shouldOnlyNormalize "ListHeadEmpty"
        , shouldOnlyNormalize "ListHeadOne"
        , shouldOnlyNormalize "ListIndexed"
        , shouldOnlyNormalize "ListIndexedEmpty"
        , shouldOnlyNormalize "ListIndexedOne"
        , shouldOnlyNormalize "ListLast"
        , shouldOnlyNormalize "ListLastEmpty"
        , shouldOnlyNormalize "ListLastOne"
        , shouldOnlyNormalize "ListLength"
        , shouldOnlyNormalize "ListLengthEmpty"
        , shouldOnlyNormalize "ListLengthOne"
        , shouldOnlyNormalize "ListNormalizeElements"
        , shouldOnlyNormalize "ListNormalizeTypeAnnotation"
        , shouldOnlyNormalize "ListReverse"
        , shouldOnlyNormalize "ListReverseEmpty"
        , shouldOnlyNormalize "ListReverseTwo"
        , shouldOnlyNormalize "Merge"
        , shouldOnlyNormalize "MergeEmptyAlternative"
        , shouldOnlyNormalize "MergeNormalizeArguments"
        , shouldOnlyNormalize "MergeWithType"
        , shouldOnlyNormalize "MergeWithTypeNormalizeArguments"
        , shouldOnlyNormalize "Natural"
        , shouldOnlyNormalize "NaturalBuild"
        , shouldOnlyNormalize "NaturalBuildFoldFusion"
        , shouldOnlyNormalize "NaturalBuildImplementation"
        , shouldOnlyNormalize "NaturalEven"
        , shouldOnlyNormalize "NaturalEvenOne"
        , shouldOnlyNormalize "NaturalEvenZero"
        , shouldOnlyNormalize "NaturalFold"
        , shouldOnlyNormalize "NaturalFoldOne"
        , shouldOnlyNormalize "NaturalFoldZero"
        , shouldOnlyNormalize "NaturalIsZero"
        , shouldOnlyNormalize "NaturalIsZeroOne"
        , shouldOnlyNormalize "NaturalIsZeroZero"
        , shouldOnlyNormalize "NaturalLiteral"
        , shouldOnlyNormalize "NaturalOdd"
        , shouldOnlyNormalize "NaturalOddOne"
        , shouldOnlyNormalize "NaturalOddZero"
        , shouldOnlyNormalize "NaturalShow"
        , shouldOnlyNormalize "NaturalShowOne"
        , shouldOnlyNormalize "NaturalToInteger"
        , shouldOnlyNormalize "NaturalToIntegerOne"
        , shouldOnlyNormalize "None"
        , shouldOnlyNormalize "NoneNatural"
        , shouldOnlyNormalize "OperatorAndEquivalentArguments"
        , shouldOnlyNormalize "OperatorAndLhsFalse"
        , shouldOnlyNormalize "OperatorAndLhsTrue"
        , shouldOnlyNormalize "OperatorAndNormalizeArguments"
        , shouldOnlyNormalize "OperatorAndRhsFalse"
        , shouldOnlyNormalize "OperatorAndRhsTrue"
        , shouldOnlyNormalize "OperatorEqualEquivalentArguments"
        , shouldOnlyNormalize "OperatorEqualLhsTrue"
        , shouldOnlyNormalize "OperatorEqualNormalizeArguments"
        , shouldOnlyNormalize "OperatorEqualRhsTrue"
        , shouldOnlyNormalize "OperatorListConcatenateLhsEmpty"
        , shouldOnlyNormalize "OperatorListConcatenateListList"
        , shouldOnlyNormalize "OperatorListConcatenateNormalizeArguments"
        , shouldOnlyNormalize "OperatorListConcatenateRhsEmpty"
        , shouldOnlyNormalize "OperatorNotEqualEquivalentArguments"
        , shouldOnlyNormalize "OperatorNotEqualLhsFalse"
        , shouldOnlyNormalize "OperatorNotEqualNormalizeArguments"
        , shouldOnlyNormalize "OperatorNotEqualRhsFalse"
        , shouldOnlyNormalize "OperatorOrEquivalentArguments"
        , shouldOnlyNormalize "OperatorOrLhsFalse"
        , shouldOnlyNormalize "OperatorOrLhsTrue"
        , shouldOnlyNormalize "OperatorOrNormalizeArguments"
        , shouldOnlyNormalize "OperatorOrRhsFalse"
        , shouldOnlyNormalize "OperatorOrRhsTrue"
        , shouldOnlyNormalize "OperatorPlusLhsZero"
        , shouldOnlyNormalize "OperatorPlusNormalizeArguments"
        , shouldOnlyNormalize "OperatorPlusOneAndOne"
        , shouldOnlyNormalize "OperatorPlusRhsZero"
        , shouldOnlyNormalize "OperatorTextConcatenateLhsEmpty"
        , shouldOnlyNormalize "OperatorTextConcatenateNormalizeArguments"
        , shouldOnlyNormalize "OperatorTextConcatenateRhsEmpty"
        , shouldOnlyNormalize "OperatorTextConcatenateTextText"
        , shouldOnlyNormalize "OperatorTimesLhsOne"
        , shouldOnlyNormalize "OperatorTimesLhsZero"
        , shouldOnlyNormalize "OperatorTimesNormalizeArguments"
        , shouldOnlyNormalize "OperatorTimesRhsOne"
        , shouldOnlyNormalize "OperatorTimesRhsZero"
        , shouldOnlyNormalize "OperatorTimesTwoAndTwo"
        , shouldOnlyNormalize "Optional"
        , shouldOnlyNormalize "OptionalBuild"
        , shouldOnlyNormalize "OptionalBuildFoldFusion"
        , shouldOnlyNormalize "OptionalBuildImplementation"
        , shouldOnlyNormalize "OptionalFold"
        , shouldOnlyNormalize "OptionalFoldNone"
        , shouldOnlyNormalize "OptionalFoldSome"
        , shouldOnlyNormalize "Record"
        , shouldOnlyNormalize "RecordEmpty"
        , shouldOnlyNormalize "RecordProjection"
        , shouldOnlyNormalize "RecordProjectionEmpty"
        , shouldOnlyNormalize "RecordProjectionNormalizeArguments"
        , shouldOnlyNormalize "RecordSelection"
        , shouldOnlyNormalize "RecordSelectionNormalizeArguments"
        , shouldOnlyNormalize "RecordType"
        , shouldOnlyNormalize "RecordTypeEmpty"
        , shouldOnlyNormalize "RecursiveRecordMergeCollision"
        , shouldOnlyNormalize "RecursiveRecordMergeLhsEmpty"
        , shouldOnlyNormalize "RecursiveRecordMergeNoCollision"
        , shouldOnlyNormalize "RecursiveRecordMergeNormalizeArguments"
        , shouldOnlyNormalize "RecursiveRecordMergeRhsEmpty"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeCollision"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeLhsEmpty"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeNoCollision"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeNormalizeArguments"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeRhsEmpty"
        , shouldOnlyNormalize "RightBiasedRecordMergeCollision"
        , shouldOnlyNormalize "RightBiasedRecordMergeLhsEmpty"
        , shouldOnlyNormalize "RightBiasedRecordMergeNoCollision"
        , shouldOnlyNormalize "RightBiasedRecordMergeNormalizeArguments"
        , shouldOnlyNormalize "RightBiasedRecordMergeRhsEmpty"
        , shouldOnlyNormalize "SomeNormalizeArguments"
        , shouldOnlyNormalize "Sort"
        , shouldOnlyNormalize "Text"
        , shouldOnlyNormalize "TextInterpolate"
        , shouldOnlyNormalize "TextLiteral"
        , shouldOnlyNormalize "TextNormalizeInterpolations"
        , shouldOnlyNormalize "TextShow"
        , shouldOnlyNormalize "TextShowAllEscapes"
        , shouldOnlyNormalize "True"
        , shouldOnlyNormalize "Type"
        , shouldOnlyNormalize "TypeAnnotation"
        , shouldOnlyNormalize "UnionNormalizeAlternatives"
        , shouldOnlyNormalize "UnionNormalizeArguments"
        , shouldOnlyNormalize "UnionProjectConstructor"
        , shouldOnlyNormalize "UnionProjectConstructorNormalizeArguments"
        , shouldOnlyNormalize "UnionSortAlternatives"
        , shouldOnlyNormalize "UnionType"
        , shouldOnlyNormalize "UnionTypeEmpty"
        , shouldOnlyNormalize "UnionTypeNormalizeArguments"
        , shouldOnlyNormalize "Variable"
        ]

alphaNormalizationTests :: TestTree
alphaNormalizationTests =
    testGroup "α-normalization tests"
        [ shouldOnlyAlphaNormalize "FunctionBindingX"
        , shouldOnlyAlphaNormalize "FunctionTypeBindingX"
        , shouldOnlyAlphaNormalize "FunctionTypeNestedBindingX"
        , shouldOnlyAlphaNormalize "FunctionNestedBindingX"
        , shouldOnlyAlphaNormalize "FunctionTypeBindingUnderscore"
        , shouldOnlyAlphaNormalize "FunctionBindingUnderscore"
        ]

simplifications :: TestTree
simplifications =
    testGroup "Simplifications"
        [ shouldNormalize "if/then/else" "./success/simplifications/ifThenElse"
        , shouldNormalize "||" "./success/simplifications/or"
        , shouldNormalize "&&" "./success/simplifications/and"
        , shouldNormalize "==" "./success/simplifications/eq"
        , shouldNormalize "!=" "./success/simplifications/ne"
        ]

constantFolding :: TestTree
constantFolding =
    testGroup "folding of constants"
        [ shouldNormalize "Natural/plus"   "success/simple/naturalPlus"
        , shouldNormalize "Optional/fold"  "success/simple/optionalFold"
        , shouldNormalize "Optional/build" "success/simple/optionalBuild"
        , shouldNormalize "Natural/build"  "success/simple/naturalBuild"
        ]

conversions :: TestTree
conversions =
    testGroup "conversions"
        [ shouldNormalize "Natural/show" "success/simple/naturalShow"
        , shouldNormalize "Integer/show" "success/simple/integerShow"
        , shouldNormalize "Double/show"  "success/simple/doubleShow"
        , shouldNormalize "Natural/toInteger" "success/simple/naturalToInteger"
        , shouldNormalize "Integer/toDouble" "success/simple/integerToDouble"
        ]

should :: Text -> Text -> TestTree
should name basename =
    Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
        let actualCode   = "./dhall-lang/tests/normalization/" <> basename <> "A.dhall"
        let expectedCode = "./dhall-lang/tests/normalization/" <> basename <> "B.dhall"

        actualExpr <- case Dhall.Parser.exprFromText mempty actualCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> pure expr

        (actualResolved, _) <- inferRoot "." actualExpr

        let actualNormalized = alphaNormalize $ nfEmpty actualResolved

        expectedExpr <- case Dhall.Parser.exprFromText mempty expectedCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> pure expr
        (expectedResolved, _) <- inferRoot "." expectedExpr

        let expectedNormalized = alphaNormalize $ nfEmpty expectedResolved

        let message =
                "The normalized expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expectedNormalized actualNormalized

shouldNormalize :: Text -> Text -> TestTree
shouldNormalize name = should ("normalize " <> name <> " correctly")

shouldOnlyAlphaNormalize :: String -> TestTree
shouldOnlyAlphaNormalize name =
    Test.Tasty.HUnit.testCase ("normalize " <> name <> " correctly") $ do

        let actualPath   = "./dhall-lang/tests/α-normalization/success/unit/" <> name <> "A.dhall"
        let expectedPath = "./dhall-lang/tests/α-normalization/success/unit/" <> name <> "B.dhall"

        actualCode   <- Data.Text.IO.readFile actualPath
        expectedCode <- Data.Text.IO.readFile expectedPath

        actualExpr <- case Dhall.Parser.exprFromText mempty actualCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> pure expr
        let actual = alphaNormalize $ denote actualExpr

        expectedExpr <- case Dhall.Parser.exprFromText mempty expectedCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> pure expr
        let expected = denote expectedExpr

        let message =
                "The normalized expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expected actual

shouldOnlyNormalize :: String -> TestTree
shouldOnlyNormalize name =
    Test.Tasty.HUnit.testCase ("normalize " <> name <> " correctly") $ do

        let actualPath   = "./dhall-lang/tests/normalization/success/unit/" <> name <> "A.dhall"
        let expectedPath = "./dhall-lang/tests/normalization/success/unit/" <> name <> "B.dhall"

        actualCode   <- Data.Text.IO.readFile actualPath
        expectedCode <- Data.Text.IO.readFile expectedPath

        actualExpr <- case Dhall.Parser.exprFromText mempty actualCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> pure expr

        let actual = alphaNormalize $ nfEmpty $ assertNoImports $ denote actualExpr

        expectedExpr <- case Dhall.Parser.exprFromText mempty expectedCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> pure expr

        let expected = alphaNormalize $ nfEmpty $ assertNoImports $ denote expectedExpr

        let message =
                "The normalized expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expected actual
