{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Normalization where

import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)

import qualified Control.Exception
import qualified Data.Text
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
        [ shouldNormalize "ListNormalizeElements" "./success/unit/ListNormalizeElementsB.dhall"
        , shouldNormalize "TextNormalizeInterpolations" "./success/unit/TextNormalizeInterpolationsB.dhall"
        , shouldNormalize "RecordTypeEmpty" "./success/unit/RecordTypeEmptyB.dhall"
        , shouldNormalize "MergeNormalizeArguments" "./success/unit/MergeNormalizeArgumentsB.dhall"
        , shouldNormalize "OperatorAndNormalizeArguments" "./success/unit/OperatorAndNormalizeArgumentsB.dhall"
        , shouldNormalize "OperatorEqualEquivalentArguments" "./success/unit/OperatorEqualEquivalentArgumentsB.dhall"
        , shouldNormalize "ListIndexedEmpty" "./success/unit/ListIndexedEmptyB.dhall"
        , shouldNormalize "OperatorListConcatenateLhsEmpty" "./success/unit/OperatorListConcatenateLhsEmptyB.dhall"
        , shouldNormalize "Variable" "./success/unit/VariableB.dhall"
        , shouldNormalize "RecursiveRecordTypeMergeRhsEmpty" "./success/unit/RecursiveRecordTypeMergeRhsEmptyB.dhall"
        , shouldNormalize "NaturalToInteger" "./success/unit/NaturalToIntegerB.dhall"
        , shouldNormalize "ListReverse" "./success/unit/ListReverseB.dhall"
        , shouldNormalize "MergeWithType" "./success/unit/MergeWithTypeB.dhall"
        , shouldNormalize "OperatorOrLhsFalse" "./success/unit/OperatorOrLhsFalseB.dhall"
        , shouldNormalize "IntegerPositive" "./success/unit/IntegerPositiveB.dhall"
        , shouldNormalize "OperatorTimesRhsOne" "./success/unit/OperatorTimesRhsOneB.dhall"
        , shouldNormalize "NaturalOdd" "./success/unit/NaturalOddB.dhall"
        , shouldNormalize "RecursiveRecordMergeLhsEmpty" "./success/unit/RecursiveRecordMergeLhsEmptyB.dhall"
        , shouldNormalize "RightBiasedRecordMergeNormalizeArguments" "./success/unit/RightBiasedRecordMergeNormalizeArgumentsB.dhall"
        , shouldNormalize "UnionType" "./success/unit/UnionTypeB.dhall"
        , shouldNormalize "IntegerNegative" "./success/unit/IntegerNegativeB.dhall"
        , shouldNormalize "NaturalEven" "./success/unit/NaturalEvenB.dhall"
        , shouldNormalize "OperatorAndRhsTrue" "./success/unit/OperatorAndRhsTrueB.dhall"
        , shouldNormalize "NaturalBuildImplementation" "./success/unit/NaturalBuildImplementationB.dhall"
        , shouldNormalize "RecursiveRecordMergeCollision" "./success/unit/RecursiveRecordMergeCollisionB.dhall"
        , shouldNormalize "RecursiveRecordMergeNormalizeArguments" "./success/unit/RecursiveRecordMergeNormalizeArgumentsB.dhall"
        , shouldNormalize "OperatorAndLhsTrue" "./success/unit/OperatorAndLhsTrueB.dhall"
        , shouldNormalize "OperatorPlusOneAndOne" "./success/unit/OperatorPlusOneAndOneB.dhall"
        , shouldNormalize "IntegerToDouble12" "./success/unit/IntegerToDouble12B.dhall"
        , shouldNormalize "IntegerToDouble-12" "./success/unit/IntegerToDouble-12B.dhall"
        , shouldNormalize "IntegerShow-12" "./success/unit/IntegerShow-12B.dhall"
        , shouldNormalize "ListLengthOne" "./success/unit/ListLengthOneB.dhall"
        , shouldNormalize "IntegerShow12" "./success/unit/IntegerShow12B.dhall"
        , shouldNormalize "NaturalFoldZero" "./success/unit/NaturalFoldZeroB.dhall"
        , shouldNormalize "RecordType" "./success/unit/RecordTypeB.dhall"
        , shouldNormalize "IfFalse" "./success/unit/IfFalseB.dhall"
        , shouldNormalize "DoubleShow" "./success/unit/DoubleShowB.dhall"
        , shouldNormalize "OptionalBuildFoldFusion" "./success/unit/OptionalBuildFoldFusionB.dhall"
        , shouldNormalize "NaturalShow" "./success/unit/NaturalShowB.dhall"
        , shouldNormalize "ListLength" "./success/unit/ListLengthB.dhall"
        , shouldNormalize "RecursiveRecordTypeMergeCollision" "./success/unit/RecursiveRecordTypeMergeCollisionB.dhall"
        , shouldNormalize "RecordEmpty" "./success/unit/RecordEmptyB.dhall"
        , shouldNormalize "TextLiteral" "./success/unit/TextLiteralB.dhall"
        , shouldNormalize "RecordSelectionNormalizeArguments" "./success/unit/RecordSelectionNormalizeArgumentsB.dhall"
        , shouldNormalize "OptionalFold" "./success/unit/OptionalFoldB.dhall"
        , shouldNormalize "IfTrivial" "./success/unit/IfTrivialB.dhall"
        , shouldNormalize "ListBuild" "./success/unit/ListBuildB.dhall"
        , shouldNormalize "OperatorTextConcatenateLhsEmpty" "./success/unit/OperatorTextConcatenateLhsEmptyB.dhall"
        , shouldNormalize "LetWithType" "./success/unit/LetWithTypeB.dhall"
        , shouldNormalize "OptionalFoldSome" "./success/unit/OptionalFoldSomeB.dhall"
        , shouldNormalize "OptionalBuild" "./success/unit/OptionalBuildB.dhall"
        , shouldNormalize "ListFoldOne" "./success/unit/ListFoldOneB.dhall"
        , shouldNormalize "ListLast" "./success/unit/ListLastB.dhall"
        , shouldNormalize "ListReverseEmpty" "./success/unit/ListReverseEmptyB.dhall"
        , shouldNormalize "NaturalEvenZero" "./success/unit/NaturalEvenZeroB.dhall"
        , shouldNormalize "RecursiveRecordTypeMergeNoCollision" "./success/unit/RecursiveRecordTypeMergeNoCollisionB.dhall"
        , shouldNormalize "NaturalToIntegerOne" "./success/unit/NaturalToIntegerOneB.dhall"
        , shouldNormalize "Natural" "./success/unit/NaturalB.dhall"
        , shouldNormalize "ListIndexed" "./success/unit/ListIndexedB.dhall"
        , shouldNormalize "Integer" "./success/unit/IntegerB.dhall"
        , shouldNormalize "NaturalIsZero" "./success/unit/NaturalIsZeroB.dhall"
        , shouldNormalize "OperatorNotEqualLhsFalse" "./success/unit/OperatorNotEqualLhsFalseB.dhall"
        , shouldNormalize "NaturalIsZeroZero" "./success/unit/NaturalIsZeroZeroB.dhall"
        , shouldNormalize "OperatorPlusNormalizeArguments" "./success/unit/OperatorPlusNormalizeArgumentsB.dhall"
        , shouldNormalize "RecordSelection" "./success/unit/RecordSelectionB.dhall"
        , shouldNormalize "OperatorListConcatenateListList" "./success/unit/OperatorListConcatenateListListB.dhall"
        , shouldNormalize "OperatorTextConcatenateNormalizeArguments" "./success/unit/OperatorTextConcatenateNormalizeArgumentsB.dhall"
        , shouldNormalize "True" "./success/unit/TrueB.dhall"
        , shouldNormalize "Bool" "./success/unit/BoolB.dhall"
        , shouldNormalize "OperatorNotEqualNormalizeArguments" "./success/unit/OperatorNotEqualNormalizeArgumentsB.dhall"
        , shouldNormalize "RightBiasedRecordMergeLhsEmpty" "./success/unit/RightBiasedRecordMergeLhsEmptyB.dhall"
        , shouldNormalize "UnionNormalizeArguments" "./success/unit/UnionNormalizeArgumentsB.dhall"
        , shouldNormalize "OperatorAndLhsFalse" "./success/unit/OperatorAndLhsFalseB.dhall"
        , shouldNormalize "RecordProjection" "./success/unit/RecordProjectionB.dhall"
        , shouldNormalize "OptionalBuildImplementation" "./success/unit/OptionalBuildImplementationB.dhall"
        , shouldNormalize "OperatorEqualNormalizeArguments" "./success/unit/OperatorEqualNormalizeArgumentsB.dhall"
        , shouldNormalize "Merge" "./success/unit/MergeB.dhall"
        , shouldNormalize "Type" "./success/unit/TypeB.dhall"
        , shouldNormalize "OperatorListConcatenateRhsEmpty" "./success/unit/OperatorListConcatenateRhsEmptyB.dhall"
        , shouldNormalize "RightBiasedRecordMergeNoCollision" "./success/unit/RightBiasedRecordMergeNoCollisionB.dhall"
        , shouldNormalize "RecursiveRecordTypeMergeLhsEmpty" "./success/unit/RecursiveRecordTypeMergeLhsEmptyB.dhall"
        , shouldNormalize "IfAlternativesIdentical" "./success/unit/IfAlternativesIdenticalB.dhall"
        , shouldNormalize "NoneNatural" "./success/unit/NoneNaturalB.dhall"
        , shouldNormalize "UnionProjectConstructor" "./success/unit/UnionProjectConstructorB.dhall"
        , shouldNormalize "ListLastOne" "./success/unit/ListLastOneB.dhall"
        , shouldNormalize "IntegerToDouble" "./success/unit/IntegerToDoubleB.dhall"
        , shouldNormalize "RightBiasedRecordMergeCollision" "./success/unit/RightBiasedRecordMergeCollisionB.dhall"
        , shouldNormalize "OperatorOrRhsTrue" "./success/unit/OperatorOrRhsTrueB.dhall"
        , shouldNormalize "IfTrue" "./success/unit/IfTrueB.dhall"
        , shouldNormalize "OperatorPlusRhsZero" "./success/unit/OperatorPlusRhsZeroB.dhall"
        , shouldNormalize "ListHeadOne" "./success/unit/ListHeadOneB.dhall"
        , shouldNormalize "FunctionApplicationNoSubstitute" "./success/unit/FunctionApplicationNoSubstituteB.dhall"
        , shouldNormalize "RecursiveRecordMergeRhsEmpty" "./success/unit/RecursiveRecordMergeRhsEmptyB.dhall"
        , shouldNormalize "ListReverseTwo" "./success/unit/ListReverseTwoB.dhall"
        , shouldNormalize "OperatorOrRhsFalse" "./success/unit/OperatorOrRhsFalseB.dhall"
        , shouldNormalize "NaturalOddZero" "./success/unit/NaturalOddZeroB.dhall"
        , shouldNormalize "UnionTypeNormalizeArguments" "./success/unit/UnionTypeNormalizeArgumentsB.dhall"
        , shouldNormalize "TextInterpolate" "./success/unit/TextInterpolateB.dhall"
        , shouldNormalize "Let" "./success/unit/LetB.dhall"
        , shouldNormalize "OperatorTimesLhsOne" "./success/unit/OperatorTimesLhsOneB.dhall"
        , shouldNormalize "OperatorOrLhsTrue" "./success/unit/OperatorOrLhsTrueB.dhall"
        , shouldNormalize "OperatorPlusLhsZero" "./success/unit/OperatorPlusLhsZeroB.dhall"
        , shouldNormalize "FunctionNormalizeArguments" "./success/unit/FunctionNormalizeArgumentsB.dhall"
        , shouldNormalize "ListLastEmpty" "./success/unit/ListLastEmptyB.dhall"
        , shouldNormalize "UnionTypeEmpty" "./success/unit/UnionTypeEmptyB.dhall"
        , shouldNormalize "TextShowAllEscapes" "./success/unit/TextShowAllEscapesB.dhall"
        , shouldNormalize "RecursiveRecordMergeNoCollision" "./success/unit/RecursiveRecordMergeNoCollisionB.dhall"
        , shouldNormalize "ListNormalizeTypeAnnotation" "./success/unit/ListNormalizeTypeAnnotationB.dhall"
        , shouldNormalize "NaturalFoldOne" "./success/unit/NaturalFoldOneB.dhall"
        , shouldNormalize "OperatorAndEquivalentArguments" "./success/unit/OperatorAndEquivalentArgumentsB.dhall"
        , shouldNormalize "SomeNormalizeArguments" "./success/unit/SomeNormalizeArgumentsB.dhall"
        , shouldNormalize "MergeWithTypeNormalizeArguments" "./success/unit/MergeWithTypeNormalizeArgumentsB.dhall"
        , shouldNormalize "OperatorOrNormalizeArguments" "./success/unit/OperatorOrNormalizeArgumentsB.dhall"
        , shouldNormalize "RecordProjectionNormalizeArguments" "./success/unit/RecordProjectionNormalizeArgumentsB.dhall"
        , shouldNormalize "FunctionApplicationSubstitute" "./success/unit/FunctionApplicationSubstituteB.dhall"
        , shouldNormalize "OperatorOrEquivalentArguments" "./success/unit/OperatorOrEquivalentArgumentsB.dhall"
        , shouldNormalize "NaturalFold" "./success/unit/NaturalFoldB.dhall"
        , shouldNormalize "NaturalEvenOne" "./success/unit/NaturalEvenOneB.dhall"
        , shouldNormalize "OperatorTextConcatenateRhsEmpty" "./success/unit/OperatorTextConcatenateRhsEmptyB.dhall"
        , shouldNormalize "DoubleLiteral" "./success/unit/DoubleLiteralB.dhall"
        , shouldNormalize "ListHeadEmpty" "./success/unit/ListHeadEmptyB.dhall"
        , shouldNormalize "FunctionApplicationCapture" "./success/unit/FunctionApplicationCaptureB.dhall"
        , shouldNormalize "RecordProjectionEmpty" "./success/unit/RecordProjectionEmptyB.dhall"
        , shouldNormalize "List" "./success/unit/ListB.dhall"
        , shouldNormalize "NaturalOddOne" "./success/unit/NaturalOddOneB.dhall"
        , shouldNormalize "ListFold" "./success/unit/ListFoldB.dhall"
        , shouldNormalize "OperatorEqualRhsTrue" "./success/unit/OperatorEqualRhsTrueB.dhall"
        , shouldNormalize "DoubleShowValue" "./success/unit/DoubleShowValueB.dhall"
        , shouldNormalize "OperatorNotEqualEquivalentArguments" "./success/unit/OperatorNotEqualEquivalentArgumentsB.dhall"
        , shouldNormalize "Text" "./success/unit/TextB.dhall"
        , shouldNormalize "ListIndexedOne" "./success/unit/ListIndexedOneB.dhall"
        , shouldNormalize "IntegerShow" "./success/unit/IntegerShowB.dhall"
        , shouldNormalize "Optional" "./success/unit/OptionalB.dhall"
        , shouldNormalize "OperatorTimesNormalizeArguments" "./success/unit/OperatorTimesNormalizeArgumentsB.dhall"
        , shouldNormalize "NaturalLiteral" "./success/unit/NaturalLiteralB.dhall"
        , shouldNormalize "ListHead" "./success/unit/ListHeadB.dhall"
        , shouldNormalize "ListBuildFoldFusion" "./success/unit/ListBuildFoldFusionB.dhall"
        , shouldNormalize "NaturalIsZeroOne" "./success/unit/NaturalIsZeroOneB.dhall"
        , shouldNormalize "OperatorListConcatenateNormalizeArguments" "./success/unit/OperatorListConcatenateNormalizeArgumentsB.dhall"
        , shouldNormalize "OperatorTimesRhsZero" "./success/unit/OperatorTimesRhsZeroB.dhall"
        , shouldNormalize "Double" "./success/unit/DoubleB.dhall"
        , shouldNormalize "OperatorTimesLhsZero" "./success/unit/OperatorTimesLhsZeroB.dhall"
        , shouldNormalize "ListFoldEmpty" "./success/unit/ListFoldEmptyB.dhall"
        , shouldNormalize "FunctionApplicationNormalizeArguments" "./success/unit/FunctionApplicationNormalizeArgumentsB.dhall"
        , shouldNormalize "NaturalShowOne" "./success/unit/NaturalShowOneB.dhall"
        , shouldNormalize "OptionalFoldNone" "./success/unit/OptionalFoldNoneB.dhall"
        , shouldNormalize "TextShow" "./success/unit/TextShowB.dhall"
        , shouldNormalize "Kind" "./success/unit/KindB.dhall"
        , shouldNormalize "Sort" "./success/unit/SortB.dhall"
        , shouldNormalize "OperatorTextConcatenateTextText" "./success/unit/OperatorTextConcatenateTextTextB.dhall"
        , shouldNormalize "FunctionTypeNormalizeArguments" "./success/unit/FunctionTypeNormalizeArgumentsB.dhall"
        , shouldNormalize "OperatorNotEqualRhsFalse" "./success/unit/OperatorNotEqualRhsFalseB.dhall"
        , shouldNormalize "NaturalBuild" "./success/unit/NaturalBuildB.dhall"
        , shouldNormalize "NaturalBuildFoldFusion" "./success/unit/NaturalBuildFoldFusionB.dhall"
        , shouldNormalize "TypeAnnotation" "./success/unit/TypeAnnotationB.dhall"
        , shouldNormalize "IfNormalizePredicateAndBranches" "./success/unit/IfNormalizePredicateAndBranchesB.dhall"
        , shouldNormalize "OperatorEqualLhsTrue" "./success/unit/OperatorEqualLhsTrueB.dhall"
        , shouldNormalize "Record" "./success/unit/RecordB.dhall"
        , shouldNormalize "RecursiveRecordTypeMergeNormalizeArguments" "./success/unit/RecursiveRecordTypeMergeNormalizeArgumentsB.dhall"
        , shouldNormalize "UnionNormalizeAlternatives" "./success/unit/UnionNormalizeAlternativesB.dhall"
        , shouldNormalize "OperatorTimesTwoAndTwo" "./success/unit/OperatorTimesTwoAndTwoB.dhall"
        , shouldNormalize "ListBuildImplementation" "./success/unit/ListBuildImplementationB.dhall"
        , shouldNormalize "UnionProjectConstructorNormalizeArguments" "./success/unit/UnionProjectConstructorNormalizeArgumentsB.dhall"
        , shouldNormalize "None" "./success/unit/NoneB.dhall"
        , shouldNormalize "RightBiasedRecordMergeRhsEmpty" "./success/unit/RightBiasedRecordMergeRhsEmptyB.dhall"
        , shouldNormalize "UnionSortAlternatives" "./success/unit/UnionSortAlternativesB.dhall"
        , shouldNormalize "ListLengthEmpty" "./success/unit/ListLengthEmptyB.dhall"
        , shouldNormalize "OperatorAndRhsFalse" "./success/unit/OperatorAndRhsFalseB.dhall"
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
