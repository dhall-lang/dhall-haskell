{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Normalization where

import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck

import Dhall.Core
import Dhall.Context
import Test.Tasty
import Test.Tasty.HUnit
import Dhall.Test.Util

tests :: TestTree
tests =
    testGroup "normalization"
        [ tutorialExamples
        , preludeExamples
        , unitTests
        , simplifications
        , constantFolding
        , conversions
        , customization
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
        , multiline
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
        [ shouldOnlyNormalize "ListNormalizeElements" "./success/unit/ListNormalizeElements"
        , shouldOnlyNormalize "TextNormalizeInterpolations" "./success/unit/TextNormalizeInterpolations"
        , shouldOnlyNormalize "RecordTypeEmpty" "./success/unit/RecordTypeEmpty"
        , shouldOnlyNormalize "MergeNormalizeArguments" "./success/unit/MergeNormalizeArguments"
        , shouldOnlyNormalize "OperatorAndNormalizeArguments" "./success/unit/OperatorAndNormalizeArguments"
        , shouldOnlyNormalize "OperatorEqualEquivalentArguments" "./success/unit/OperatorEqualEquivalentArguments"
        , shouldOnlyNormalize "ListIndexedEmpty" "./success/unit/ListIndexedEmpty"
        , shouldOnlyNormalize "OperatorListConcatenateLhsEmpty" "./success/unit/OperatorListConcatenateLhsEmpty"
        , shouldOnlyNormalize "Variable" "./success/unit/Variable"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeRhsEmpty" "./success/unit/RecursiveRecordTypeMergeRhsEmpty"
        , shouldOnlyNormalize "NaturalToInteger" "./success/unit/NaturalToInteger"
        , shouldOnlyNormalize "ListReverse" "./success/unit/ListReverse"
        , shouldOnlyNormalize "MergeWithType" "./success/unit/MergeWithType"
        , shouldOnlyNormalize "OperatorOrLhsFalse" "./success/unit/OperatorOrLhsFalse"
        , shouldOnlyNormalize "IntegerPositive" "./success/unit/IntegerPositive"
        , shouldOnlyNormalize "OperatorTimesRhsOne" "./success/unit/OperatorTimesRhsOne"
        , shouldOnlyNormalize "NaturalOdd" "./success/unit/NaturalOdd"
        , shouldOnlyNormalize "RecursiveRecordMergeLhsEmpty" "./success/unit/RecursiveRecordMergeLhsEmpty"
        , shouldOnlyNormalize "RightBiasedRecordMergeNormalizeArguments" "./success/unit/RightBiasedRecordMergeNormalizeArguments"
        , shouldOnlyNormalize "UnionType" "./success/unit/UnionType"
        , shouldOnlyNormalize "IntegerNegative" "./success/unit/IntegerNegative"
        , shouldOnlyNormalize "NaturalEven" "./success/unit/NaturalEven"
        , shouldOnlyNormalize "OperatorAndRhsTrue" "./success/unit/OperatorAndRhsTrue"
        , shouldOnlyNormalize "NaturalBuildImplementation" "./success/unit/NaturalBuildImplementation"
        , shouldOnlyNormalize "RecursiveRecordMergeCollision" "./success/unit/RecursiveRecordMergeCollision"
        , shouldOnlyNormalize "RecursiveRecordMergeNormalizeArguments" "./success/unit/RecursiveRecordMergeNormalizeArguments"
        , shouldOnlyNormalize "OperatorAndLhsTrue" "./success/unit/OperatorAndLhsTrue"
        , shouldOnlyNormalize "OperatorPlusOneAndOne" "./success/unit/OperatorPlusOneAndOne"
        , shouldOnlyNormalize "IntegerToDouble12" "./success/unit/IntegerToDouble12"
        , shouldOnlyNormalize "IntegerToDouble-12" "./success/unit/IntegerToDouble-12"
        , shouldOnlyNormalize "IntegerShow-12" "./success/unit/IntegerShow-12"
        , shouldOnlyNormalize "ListLengthOne" "./success/unit/ListLengthOne"
        , shouldOnlyNormalize "IntegerShow12" "./success/unit/IntegerShow12"
        , shouldOnlyNormalize "NaturalFoldZero" "./success/unit/NaturalFoldZero"
        , shouldOnlyNormalize "RecordType" "./success/unit/RecordType"
        , shouldOnlyNormalize "IfFalse" "./success/unit/IfFalse"
        , shouldOnlyNormalize "DoubleShow" "./success/unit/DoubleShow"
        , shouldOnlyNormalize "OptionalBuildFoldFusion" "./success/unit/OptionalBuildFoldFusion"
        , shouldOnlyNormalize "NaturalShow" "./success/unit/NaturalShow"
        , shouldOnlyNormalize "ListLength" "./success/unit/ListLength"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeCollision" "./success/unit/RecursiveRecordTypeMergeCollision"
        , shouldOnlyNormalize "RecordEmpty" "./success/unit/RecordEmpty"
        , shouldOnlyNormalize "TextLiteral" "./success/unit/TextLiteral"
        , shouldOnlyNormalize "RecordSelectionNormalizeArguments" "./success/unit/RecordSelectionNormalizeArguments"
        , shouldOnlyNormalize "OptionalFold" "./success/unit/OptionalFold"
        , shouldOnlyNormalize "IfTrivial" "./success/unit/IfTrivial"
        , shouldOnlyNormalize "ListBuild" "./success/unit/ListBuild"
        , shouldOnlyNormalize "OperatorTextConcatenateLhsEmpty" "./success/unit/OperatorTextConcatenateLhsEmpty"
        , shouldOnlyNormalize "LetWithType" "./success/unit/LetWithType"
        , shouldOnlyNormalize "OptionalFoldSome" "./success/unit/OptionalFoldSome"
        , shouldOnlyNormalize "OptionalBuild" "./success/unit/OptionalBuild"
        , shouldOnlyNormalize "ListFoldOne" "./success/unit/ListFoldOne"
        , shouldOnlyNormalize "ListLast" "./success/unit/ListLast"
        , shouldOnlyNormalize "ListReverseEmpty" "./success/unit/ListReverseEmpty"
        , shouldOnlyNormalize "NaturalEvenZero" "./success/unit/NaturalEvenZero"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeNoCollision" "./success/unit/RecursiveRecordTypeMergeNoCollision"
        , shouldOnlyNormalize "NaturalToIntegerOne" "./success/unit/NaturalToIntegerOne"
        , shouldOnlyNormalize "Natural" "./success/unit/Natural"
        , shouldOnlyNormalize "ListIndexed" "./success/unit/ListIndexed"
        , shouldOnlyNormalize "Integer" "./success/unit/Integer"
        , shouldOnlyNormalize "NaturalIsZero" "./success/unit/NaturalIsZero"
        , shouldOnlyNormalize "OperatorNotEqualLhsFalse" "./success/unit/OperatorNotEqualLhsFalse"
        , shouldOnlyNormalize "NaturalIsZeroZero" "./success/unit/NaturalIsZeroZero"
        , shouldOnlyNormalize "OperatorPlusNormalizeArguments" "./success/unit/OperatorPlusNormalizeArguments"
        , shouldOnlyNormalize "RecordSelection" "./success/unit/RecordSelection"
        , shouldOnlyNormalize "OperatorListConcatenateListList" "./success/unit/OperatorListConcatenateListList"
        , shouldOnlyNormalize "OperatorTextConcatenateNormalizeArguments" "./success/unit/OperatorTextConcatenateNormalizeArguments"
        , shouldOnlyNormalize "True" "./success/unit/True"
        , shouldOnlyNormalize "Bool" "./success/unit/Bool"
        , shouldOnlyNormalize "OperatorNotEqualNormalizeArguments" "./success/unit/OperatorNotEqualNormalizeArguments"
        , shouldOnlyNormalize "RightBiasedRecordMergeLhsEmpty" "./success/unit/RightBiasedRecordMergeLhsEmpty"
        , shouldOnlyNormalize "UnionNormalizeArguments" "./success/unit/UnionNormalizeArguments"
        , shouldOnlyNormalize "OperatorAndLhsFalse" "./success/unit/OperatorAndLhsFalse"
        , shouldOnlyNormalize "RecordProjection" "./success/unit/RecordProjection"
        , shouldOnlyNormalize "OptionalBuildImplementation" "./success/unit/OptionalBuildImplementation"
        , shouldOnlyNormalize "OperatorEqualNormalizeArguments" "./success/unit/OperatorEqualNormalizeArguments"
        , shouldOnlyNormalize "Merge" "./success/unit/Merge"
        , shouldOnlyNormalize "Type" "./success/unit/Type"
        , shouldOnlyNormalize "OperatorListConcatenateRhsEmpty" "./success/unit/OperatorListConcatenateRhsEmpty"
        , shouldOnlyNormalize "RightBiasedRecordMergeNoCollision" "./success/unit/RightBiasedRecordMergeNoCollision"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeLhsEmpty" "./success/unit/RecursiveRecordTypeMergeLhsEmpty"
        , shouldOnlyNormalize "IfAlternativesIdentical" "./success/unit/IfAlternativesIdentical"
        , shouldOnlyNormalize "NoneNatural" "./success/unit/NoneNatural"
        , shouldOnlyNormalize "UnionProjectConstructor" "./success/unit/UnionProjectConstructor"
        , shouldOnlyNormalize "ListLastOne" "./success/unit/ListLastOne"
        , shouldOnlyNormalize "IntegerToDouble" "./success/unit/IntegerToDouble"
        , shouldOnlyNormalize "RightBiasedRecordMergeCollision" "./success/unit/RightBiasedRecordMergeCollision"
        , shouldOnlyNormalize "OperatorOrRhsTrue" "./success/unit/OperatorOrRhsTrue"
        , shouldOnlyNormalize "IfTrue" "./success/unit/IfTrue"
        , shouldOnlyNormalize "OperatorPlusRhsZero" "./success/unit/OperatorPlusRhsZero"
        , shouldOnlyNormalize "ListHeadOne" "./success/unit/ListHeadOne"
        , shouldOnlyNormalize "FunctionApplicationNoSubstitute" "./success/unit/FunctionApplicationNoSubstitute"
        , shouldOnlyNormalize "RecursiveRecordMergeRhsEmpty" "./success/unit/RecursiveRecordMergeRhsEmpty"
        , shouldOnlyNormalize "ListReverseTwo" "./success/unit/ListReverseTwo"
        , shouldOnlyNormalize "OperatorOrRhsFalse" "./success/unit/OperatorOrRhsFalse"
        , shouldOnlyNormalize "NaturalOddZero" "./success/unit/NaturalOddZero"
        , shouldOnlyNormalize "UnionTypeNormalizeArguments" "./success/unit/UnionTypeNormalizeArguments"
        , shouldOnlyNormalize "TextInterpolate" "./success/unit/TextInterpolate"
        , shouldOnlyNormalize "Let" "./success/unit/Let"
        , shouldOnlyNormalize "OperatorTimesLhsOne" "./success/unit/OperatorTimesLhsOne"
        , shouldOnlyNormalize "OperatorOrLhsTrue" "./success/unit/OperatorOrLhsTrue"
        , shouldOnlyNormalize "OperatorPlusLhsZero" "./success/unit/OperatorPlusLhsZero"
        , shouldOnlyNormalize "FunctionNormalizeArguments" "./success/unit/FunctionNormalizeArguments"
        , shouldOnlyNormalize "ListLastEmpty" "./success/unit/ListLastEmpty"
        , shouldOnlyNormalize "UnionTypeEmpty" "./success/unit/UnionTypeEmpty"
        , shouldOnlyNormalize "TextShowAllEscapes" "./success/unit/TextShowAllEscapes"
        , shouldOnlyNormalize "RecursiveRecordMergeNoCollision" "./success/unit/RecursiveRecordMergeNoCollision"
        , shouldOnlyNormalize "ListNormalizeTypeAnnotation" "./success/unit/ListNormalizeTypeAnnotation"
        , shouldOnlyNormalize "NaturalFoldOne" "./success/unit/NaturalFoldOne"
        , shouldOnlyNormalize "OperatorAndEquivalentArguments" "./success/unit/OperatorAndEquivalentArguments"
        , shouldOnlyNormalize "SomeNormalizeArguments" "./success/unit/SomeNormalizeArguments"
        , shouldOnlyNormalize "MergeWithTypeNormalizeArguments" "./success/unit/MergeWithTypeNormalizeArguments"
        , shouldOnlyNormalize "OperatorOrNormalizeArguments" "./success/unit/OperatorOrNormalizeArguments"
        , shouldOnlyNormalize "RecordProjectionNormalizeArguments" "./success/unit/RecordProjectionNormalizeArguments"
        , shouldOnlyNormalize "FunctionApplicationSubstitute" "./success/unit/FunctionApplicationSubstitute"
        , shouldOnlyNormalize "OperatorOrEquivalentArguments" "./success/unit/OperatorOrEquivalentArguments"
        , shouldOnlyNormalize "NaturalFold" "./success/unit/NaturalFold"
        , shouldOnlyNormalize "NaturalEvenOne" "./success/unit/NaturalEvenOne"
        , shouldOnlyNormalize "OperatorTextConcatenateRhsEmpty" "./success/unit/OperatorTextConcatenateRhsEmpty"
        , shouldOnlyNormalize "DoubleLiteral" "./success/unit/DoubleLiteral"
        , shouldOnlyNormalize "ListHeadEmpty" "./success/unit/ListHeadEmpty"
        , shouldOnlyNormalize "FunctionApplicationCapture" "./success/unit/FunctionApplicationCapture"
        , shouldOnlyNormalize "RecordProjectionEmpty" "./success/unit/RecordProjectionEmpty"
        , shouldOnlyNormalize "List" "./success/unit/List"
        , shouldOnlyNormalize "NaturalOddOne" "./success/unit/NaturalOddOne"
        , shouldOnlyNormalize "ListFold" "./success/unit/ListFold"
        , shouldOnlyNormalize "OperatorEqualRhsTrue" "./success/unit/OperatorEqualRhsTrue"
        , shouldOnlyNormalize "DoubleShowValue" "./success/unit/DoubleShowValue"
        , shouldOnlyNormalize "OperatorNotEqualEquivalentArguments" "./success/unit/OperatorNotEqualEquivalentArguments"
        , shouldOnlyNormalize "Text" "./success/unit/Text"
        , shouldOnlyNormalize "ListIndexedOne" "./success/unit/ListIndexedOne"
        , shouldOnlyNormalize "IntegerShow" "./success/unit/IntegerShow"
        , shouldOnlyNormalize "Optional" "./success/unit/Optional"
        , shouldOnlyNormalize "OperatorTimesNormalizeArguments" "./success/unit/OperatorTimesNormalizeArguments"
        , shouldOnlyNormalize "NaturalLiteral" "./success/unit/NaturalLiteral"
        , shouldOnlyNormalize "ListHead" "./success/unit/ListHead"
        , shouldOnlyNormalize "ListBuildFoldFusion" "./success/unit/ListBuildFoldFusion"
        , shouldOnlyNormalize "NaturalIsZeroOne" "./success/unit/NaturalIsZeroOne"
        , shouldOnlyNormalize "OperatorListConcatenateNormalizeArguments" "./success/unit/OperatorListConcatenateNormalizeArguments"
        , shouldOnlyNormalize "OperatorTimesRhsZero" "./success/unit/OperatorTimesRhsZero"
        , shouldOnlyNormalize "Double" "./success/unit/Double"
        , shouldOnlyNormalize "OperatorTimesLhsZero" "./success/unit/OperatorTimesLhsZero"
        , shouldOnlyNormalize "ListFoldEmpty" "./success/unit/ListFoldEmpty"
        , shouldOnlyNormalize "FunctionApplicationNormalizeArguments" "./success/unit/FunctionApplicationNormalizeArguments"
        , shouldOnlyNormalize "NaturalShowOne" "./success/unit/NaturalShowOne"
        , shouldOnlyNormalize "OptionalFoldNone" "./success/unit/OptionalFoldNone"
        , shouldOnlyNormalize "TextShow" "./success/unit/TextShow"
        , shouldOnlyNormalize "Kind" "./success/unit/Kind"
        , shouldOnlyNormalize "Sort" "./success/unit/Sort"
        , shouldOnlyNormalize "OperatorTextConcatenateTextText" "./success/unit/OperatorTextConcatenateTextText"
        , shouldOnlyNormalize "FunctionTypeNormalizeArguments" "./success/unit/FunctionTypeNormalizeArguments"
        , shouldOnlyNormalize "OperatorNotEqualRhsFalse" "./success/unit/OperatorNotEqualRhsFalse"
        , shouldOnlyNormalize "NaturalBuild" "./success/unit/NaturalBuild"
        , shouldOnlyNormalize "NaturalBuildFoldFusion" "./success/unit/NaturalBuildFoldFusion"
        , shouldOnlyNormalize "TypeAnnotation" "./success/unit/TypeAnnotation"
        , shouldOnlyNormalize "IfNormalizePredicateAndBranches" "./success/unit/IfNormalizePredicateAndBranches"
        , shouldOnlyNormalize "OperatorEqualLhsTrue" "./success/unit/OperatorEqualLhsTrue"
        , shouldOnlyNormalize "Record" "./success/unit/Record"
        , shouldOnlyNormalize "RecursiveRecordTypeMergeNormalizeArguments" "./success/unit/RecursiveRecordTypeMergeNormalizeArguments"
        , shouldOnlyNormalize "UnionNormalizeAlternatives" "./success/unit/UnionNormalizeAlternatives"
        , shouldOnlyNormalize "OperatorTimesTwoAndTwo" "./success/unit/OperatorTimesTwoAndTwo"
        , shouldOnlyNormalize "ListBuildImplementation" "./success/unit/ListBuildImplementation"
        , shouldOnlyNormalize "UnionProjectConstructorNormalizeArguments" "./success/unit/UnionProjectConstructorNormalizeArguments"
        , shouldOnlyNormalize "None" "./success/unit/None"
        , shouldOnlyNormalize "RightBiasedRecordMergeRhsEmpty" "./success/unit/RightBiasedRecordMergeRhsEmpty"
        , shouldOnlyNormalize "UnionSortAlternatives" "./success/unit/UnionSortAlternatives"
        , shouldOnlyNormalize "ListLengthEmpty" "./success/unit/ListLengthEmpty"
        , shouldOnlyNormalize "OperatorAndRhsFalse" "./success/unit/OperatorAndRhsFalse"
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

customization :: TestTree
customization =
    testGroup "customization"
        [ simpleCustomization
        , nestedReduction
        ]

multiline :: TestTree
multiline =
    testGroup "Multi-line literals"
        [ shouldNormalize
            "multi-line escape sequences"
            "./success/multiline/escape"
        , shouldNormalize
            "a multi-line literal with a hanging indent"
            "./success/multiline/hangingIndent"
        , shouldNormalize
            "a multi-line literal with an interior indent"
            "./success/multiline/interiorIndent"
        , shouldNormalize
            "a multi-line literal with an interpolated expression"
            "./success/multiline/interpolation"
        , should
            "preserve comments within a multi-line literal"
            "./success/multiline/preserveComment"
        , shouldNormalize
            "a multi-line literal with one line"
            "./success/multiline/singleLine"
        , shouldNormalize
            "a multi-line literal with two lines"
            "./success/multiline/twoLines"
        ]

simpleCustomization :: TestTree
simpleCustomization = testCase "simpleCustomization" $ do
  let tyCtx  = insert "min" (Pi "_" Natural (Pi "_" Natural Natural)) empty
      valCtx e = case e of
                    (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y)) -> pure (Just (NaturalLit (min x y)))
                    _ -> pure Nothing
  e <- codeWith tyCtx "min (min 11 12) 8 + 1"
  assertNormalizesToWith valCtx e "9"

nestedReduction :: TestTree
nestedReduction = testCase "doubleReduction" $ do
  minType        <- insert "min"        <$> code "Natural → Natural → Natural"
  fiveorlessType <- insert "fiveorless" <$> code "Natural → Natural"
  wurbleType     <- insert "wurble"     <$> code "Natural → Integer"
  let tyCtx = minType . fiveorlessType . wurbleType $ empty
      valCtx e = case e of
                    (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y)) -> pure (Just (NaturalLit (min x y)))
                    (App (Var (V "wurble" 0)) (NaturalLit x)) -> pure (Just
                        (App (Var (V "fiveorless" 0)) (NaturalPlus (NaturalLit x) (NaturalLit 2))))
                    (App (Var (V "fiveorless" 0)) (NaturalLit x)) -> pure (Just
                        (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalPlus (NaturalLit 3) (NaturalLit 2))))
                    _ -> pure Nothing
  e <- codeWith tyCtx "wurble 6"
  assertNormalizesToWith valCtx e "5"

should :: Text -> Text -> TestTree
should name basename =
    Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
        let actualCode   = "./dhall-lang/tests/normalization/" <> basename <> "A.dhall"
        let expectedCode = "./dhall-lang/tests/normalization/" <> basename <> "B.dhall"

        actualExpr <- case Dhall.Parser.exprFromText mempty actualCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        actualResolved <- Dhall.Import.load actualExpr
        case Dhall.TypeCheck.typeOf actualResolved of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()
        let actualNormalized =
                Dhall.Core.alphaNormalize
                    (Dhall.Core.normalize actualResolved :: Expr X X)

        expectedExpr <- case Dhall.Parser.exprFromText mempty expectedCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        expectedResolved <- Dhall.Import.load expectedExpr
        case Dhall.TypeCheck.typeOf expectedResolved of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()
        -- Use `denote` instead of `normalize` to enforce that the expected
        -- expression is already in normal form
        let expectedNormalized =
                Dhall.Core.alphaNormalize (Dhall.Core.denote expectedResolved)

        let message =
                "The normalized expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expectedNormalized actualNormalized

shouldNormalize :: Text -> Text -> TestTree
shouldNormalize name = should ("normalize " <> name <> " correctly")

shouldOnlyNormalize :: Text -> String -> TestTree
shouldOnlyNormalize name basename =
    Test.Tasty.HUnit.testCase (Data.Text.unpack ("normalize " <> name <> " correctly")) $ do

        let actualPath   = "./dhall-lang/tests/normalization/" <> basename <> "A.dhall"
        let expectedPath = "./dhall-lang/tests/normalization/" <> basename <> "B.dhall"

        actualCode   <- Data.Text.IO.readFile actualPath
        expectedCode <- Data.Text.IO.readFile expectedPath

        actualExpr <- case Dhall.Parser.exprFromText mempty actualCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        actualResolved <- Dhall.Import.assertNoImports actualExpr

        let actualNormalized =
                Dhall.Core.alphaNormalize
                    (Dhall.Core.normalize actualResolved :: Expr X X)

        expectedExpr <- case Dhall.Parser.exprFromText mempty expectedCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        expectedResolved <- Dhall.Import.assertNoImports expectedExpr

        -- Use `denote` instead of `normalize` to enforce that the expected
        -- expression is already in normal form
        let expectedNormalized =
                Dhall.Core.alphaNormalize (Dhall.Core.denote expectedResolved)

        let message =
                "The normalized expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expectedNormalized actualNormalized
