{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Regression where

import Data.Either.Validation (Validation (..))
import Data.Void              (Void)
import Dhall.Core             (Expr (..), ReifiedNormalizer (..))
import Dhall.Import           (Imported, MissingImports (..))
import Dhall.Parser           (SourcedException (..), Src)
import Dhall.TypeCheck        (TypeError)
import Test.Tasty             (TestTree)
import Test.Tasty.HUnit       ((@?=))
import Data.Text              (Text)

import qualified Control.Exception
import qualified Data.Text.IO
import qualified Data.Text.Lazy.IO
import qualified Dhall
import qualified Dhall.Context             as Context
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.Test.Util           as Util
import qualified Dhall.TypeCheck
import qualified Lens.Micro                as Lens
import qualified Prettyprinter
import qualified Prettyprinter.Render.Text
import qualified System.Timeout
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import Dhall.TypeCheck (TypeError(typeMessage))
import Dhall.TypeCheck (prettyTypeMessage)
import qualified Data.Text as Text
import Dhall.TypeCheck (short)
import qualified Data.List as List
import qualified Prettyprinter as Pretty
import   Prettyprinter.Render.Text (renderStrict)     
import Dhall.TypeCheck (ErrorMessages(long))


tests :: TestTree
tests =
    Test.Tasty.testGroup "regression tests"
        [ issue96
        , issue126
        , issue151
        , issue164
        , issue201
        , issue216
        , issue253
        , issue1131a
        , issue1131b
        , issue1341
        , issue1584
        , issue1646
        , issue1732
        , issue1884
        , issue2088
        , issue2463
        , parsing0
        , typeChecking0
        , typeChecking1
        , typeChecking2
        , unnamedFields
        , trailingSpaceAfterStringLiterals
        , combineTypes
        ]

data Foo = Foo Integer Bool | Bar Bool Bool Bool | Baz Integer Integer
    deriving (Show, Dhall.Generic, Dhall.FromDhall, Dhall.ToDhall)

unnamedFields :: TestTree
unnamedFields = Test.Tasty.HUnit.testCase "Unnamed Fields" (do
    let ty = Dhall.auto :: Dhall.Decoder Foo
    Test.Tasty.HUnit.assertEqual "Good type" (Dhall.expected ty)
        (Success (Dhall.Core.Union
            (Dhall.Map.fromList
                [   ("Foo",Just (Dhall.Core.Record (Dhall.Map.fromList
                        [ ("_1", Dhall.Core.makeRecordField Dhall.Core.Integer)
                        , ("_2", Dhall.Core.makeRecordField Dhall.Core.Bool)
                        ])))
                ,   ("Bar",Just (Dhall.Core.Record (Dhall.Map.fromList
                        [ ("_1", Dhall.Core.makeRecordField Dhall.Core.Bool)
                        , ("_2", Dhall.Core.makeRecordField Dhall.Core.Bool)
                        , ("_3", Dhall.Core.makeRecordField Dhall.Core.Bool)
                        ])))
                ,   ("Baz",Just (Dhall.Core.Record (Dhall.Map.fromList
                        [ ("_1", Dhall.Core.makeRecordField Dhall.Core.Integer)
                        , ("_2", Dhall.Core.makeRecordField Dhall.Core.Integer)
                        ])))
                ]
            )
        ))

    let inj = Dhall.inject :: Dhall.Encoder Foo
    Test.Tasty.HUnit.assertEqual "Good ToDhall" (Success $ Dhall.declared inj) (Dhall.expected ty)

    let tu_ty = Dhall.auto :: Dhall.Decoder (Integer, Bool)
    Test.Tasty.HUnit.assertEqual "Auto Tuple" (Dhall.expected tu_ty) (Success $ Dhall.Core.Record (
            Dhall.Map.fromList
                [ ("_1", Dhall.Core.makeRecordField Dhall.Core.Integer)
                , ("_2", Dhall.Core.makeRecordField Dhall.Core.Bool)
                ]))

    let tu_in = Dhall.inject :: Dhall.Encoder (Integer, Bool)
    Test.Tasty.HUnit.assertEqual "Inj. Tuple" (Success $ Dhall.declared tu_in) (Dhall.expected tu_ty)

    return () )

issue96 :: TestTree
issue96 = Test.Tasty.HUnit.testCase "Issue #96" (do
    -- Verify that parsing should not fail
    _ <- Util.code "\"bar'baz\""
    return () )

issue126 :: TestTree
issue126 = Test.Tasty.HUnit.testCase "Issue #126" (do
    e <- Util.code "''\nfoo\nbar\n''"
    Util.normalize' e @?= "''\nfoo\nbar\n''" )

issue151 :: TestTree
issue151 = Test.Tasty.HUnit.testCase "Issue #151" (do
    let shouldNotTypeCheck text = do
            let handler :: SourcedException MissingImports -> IO Bool
                handler (SourcedException _ (MissingImports [e])) =
                    case Control.Exception.fromException e :: Maybe (Imported (TypeError Src Void)) of
                        Just _ -> return True
                        Nothing -> return False
                handler _ =
                    return True

            let typeCheck = do
                    _ <- Util.code text
                    return False
            b <- Control.Exception.handle handler typeCheck
            Test.Tasty.HUnit.assertBool "The expression should not type-check" b

    -- These two examples contain the following expression that loops infinitely
    -- if you normalize the expression before type-checking the expression:
    --
    --     (λ(x : A) → x x) (λ(x : A) → x x)
    --
    -- There was a bug in the Dhall type-checker were expressions were not
    -- being type-checked before being added to the context (which is a
    -- violation of the standard type-checking rules for a pure type system).
    -- The reason this is problematic is that several places in the
    -- type-checking logic assume that the context is safe to normalize.
    --
    -- Both of these examples exercise area of the type-checker logic that
    -- assume that the context is normalized.  If you fail to type-check terms
    -- before adding them to the context then this test will "fail" with an
    -- infinite loop.  This test "succeeds" if the examples terminate
    -- immediately with a type-checking failure.
    shouldNotTypeCheck "./tests/regression/issue151a.dhall"
    shouldNotTypeCheck "./tests/regression/issue151b.dhall" )

issue164 :: TestTree
issue164 = Test.Tasty.HUnit.testCase "Issue #164" (do
    -- Verify that parsing should not fail on a single-quote within a
    -- single-quoted string
    _ <- Util.code "./tests/regression/issue164.dhall"
    return () )

issue201 :: TestTree
issue201 = Test.Tasty.HUnit.testCase "Issue #201" (do
    -- Verify that type synonyms work
    _ <- Util.code "./tests/regression/issue201.dhall"
    return () )

issue216 :: TestTree
issue216 = Test.Tasty.HUnit.testCase "Issue #216" (do
    -- Verify that pretty-printing preserves string interpolation
    source <- Data.Text.IO.readFile "./tests/regression/issue216b.dhall"
    e <- Util.code source
    let doc       = Prettyprinter.pretty e
    let docStream = Dhall.Pretty.layout doc
    let text0 = Prettyprinter.Render.Text.renderLazy docStream

    text1 <- Data.Text.Lazy.IO.readFile "./tests/regression/issue216b.dhall"

    Test.Tasty.HUnit.assertEqual "Pretty-printing should preserve string interpolation" text1 text0 )

issue253 :: TestTree
issue253 = Test.Tasty.HUnit.testCase "Issue #253" (do
    -- Verify that type-checking rejects ill-formed custom contexts
    let context = Context.insert "x" "x" Context.empty
    let result = Dhall.TypeCheck.typeWith context "x"

    -- If the context is not validated correctly then type-checking will
    -- infinitely loop
    Just _ <- System.Timeout.timeout 1000000 (Control.Exception.evaluate $! result)
    return () )

issue1131a :: TestTree
issue1131a = Test.Tasty.HUnit.testCase "Issue #1131 a"
    (Util.assertDoesntTypeCheck "toMap {=} : <>.x")

issue1131b :: TestTree
issue1131b = Test.Tasty.HUnit.testCase "Issue #1131 b"
    (Util.assertDoesntTypeCheck "toMap {=} : List {mapKey : Text, mapValue : 0}")

issue1341 :: TestTree
issue1341 = Test.Tasty.HUnit.testCase "Issue #1341" (do
    let nan    = Dhall.Core.DoubleLit (Dhall.Core.DhallDouble (0/0)) :: Dhall.Core.Expr () ()
    let actual = Dhall.Core.V "x" 0 `Dhall.Core.freeIn` nan
    let msg    = "NaN shouldn't contain any free variables"
    Test.Tasty.HUnit.assertEqual msg False actual)

issue1584 :: TestTree
issue1584 = Test.Tasty.HUnit.testCase "Issue #1584" (do
    -- This test ensures that we can parse variables with keyword prefixes
    -- (e.g. `ifX`)
    _ <- Util.code "./tests/regression/issue1584.dhall"
    return () )

issue1646 :: TestTree
issue1646 = Test.Tasty.HUnit.testCase "Issue #1646" (do
    -- This test ensures that the parser doesn't eagerly consume trailing
    -- whitespace after a `Double`
    _ <- Util.code "./tests/regression/issue1646.dhall"
    return () )

issue1732 :: TestTree
issue1732 = Test.Tasty.HUnit.testCase "Issue #1732" (do
    -- This test ensures that the parser allows whitespace after a record pun
    -- entry
    _ <- Util.code "./tests/regression/issue1732.dhall"
    return () )

issue1884 :: TestTree
issue1884 = Test.Tasty.HUnit.testCase "Issue #1884" (do
    -- This test ensures that the parser allows a parenthesized application
    -- expression as the first argument to a with expression
    _ <- Util.code "./tests/regression/issue1884.dhall"
    return () )

issue2088 :: TestTree
issue2088 = Test.Tasty.HUnit.testCase "Issue #2088" (do
    -- This test ensures that the parser for projection by labels doesn't
    -- accidentally swallow trailing commas OUTSIDE of the projection
    _ <- Util.code "./tests/regression/issue2088.dhall"
    return () )

issue2463 :: TestTree
issue2463 = Test.Tasty.HUnit.testCase "Issue #2463" (do
    let startingContext = Context.insert "foo" Integer Context.empty

    let normalizer (Var "foo") = Just (IntegerLit 0)
        normalizer  _          = Nothing

    let inputSettings = transform Dhall.defaultInputSettings
          where
            transform =
                  Lens.set Dhall.normalizer
                      (Just (ReifiedNormalizer (pure . normalizer)))
                . Lens.set Dhall.startingContext startingContext

    x <- Dhall.inputWithSettings inputSettings Dhall.auto "foo"

    Test.Tasty.HUnit.assertEqual "" (0 :: Integer) x )

parsing0 :: TestTree
parsing0 = Test.Tasty.HUnit.testCase "Parsing regression #0" (
    -- Verify that parsing should not fail
    --
    -- In 267093f8cddf1c2f909f2d997c31fd0a7cb2440a I broke the parser when left
    -- factoring the grammar by failing to handle the source tested by this
    -- regression test.  The root of the problem was that the parser was trying
    -- to parse `List ./Node` as `Field List "/Node"` instead of
    -- `App List (Embed (Path (File Homeless "./Node") Code))`.  The latter is
    -- the correct parse because `/Node` is not a valid field label, but the
    -- mistaken parser was committed to the incorrect parse and never attempted
    -- the correct parse.
    case Dhall.Parser.exprFromText mempty "List ./Node" of
        Left  e -> Control.Exception.throwIO e
        Right _ -> return () )

typeChecking0 :: TestTree
typeChecking0 = Test.Tasty.HUnit.testCase "Type-checking regression #0" (do
    -- There used to be a bug in the type-checking logic where `T` would be
    -- added to the context when inferring the type of `λ(x : T) → x`, but was
    -- missing from the context when inferring the kind of the inferred type
    -- (i.e. the kind of `∀(x : T) → T`).  This led to an unbound variable
    -- error when inferring the kind
    --
    -- This bug was originally reported in issue #10
    _ <- Util.code "let Day : Type = Natural in λ(x : Day) → x"
    return () )

typeChecking1 :: TestTree
typeChecking1 = Test.Tasty.HUnit.testCase "Type-checking regression #1" (do
    -- There used to be a bug in the type-checking logic when inferring the
    -- type of `let`.  Specifically, given an expression of the form:
    --
    --     let x : t = e1 in e2
    --
    -- ... the type-checker would not substitute `x` for `e1` in the inferred
    -- of the `let` expression.  This meant that if the inferred type contained
    -- any references to `x` then these references would escape their scope and
    -- result in unbound variable exceptions
    --
    -- This regression test exercises that code path by creating a `let`
    -- expression where the inferred type before substitution (`∀(x : b) → b` in
    -- this example), contains a reference to the `let`-bound variable (`b`)
    -- that needs to be substituted with `a` in order to produce the final
    -- correct inferred type (`∀(x : a) → a`).  If this test passes then
    -- substitution worked correctly.  If substitution doesn't occur then you
    -- expect an `Unbound variable` error from `b` escaping its scope due to
    -- being returned as part of the inferred type
    _ <- Util.code "λ(a : Type) → let b : Type = a in λ(x : b) → x"
    return () )

typeChecking2 :: TestTree
typeChecking2 = Test.Tasty.HUnit.testCase "Type-checking regression #2" (do
    -- There used to be a bug in the type-checking logic where `let` bound
    -- variables would not correctly shadow variables of the same name when
    -- inferring the type of of the `let` expression
    --
    -- This example exercises this code path with a `let` expression where the
    -- `let`-bound variable (`a`) has the same name as its own type (`a`).  If
    -- shadowing works correctly then the final `a` in the expression should
    -- refer to the `let`-bound variable and not its type.  An invalid reference
    -- to the shadowed type `a` would result in an invalid dependently typed
    -- function
    _ <- Util.code "λ(a : Type) → λ(x : a) → let a : a = x in a"
    return () )

trailingSpaceAfterStringLiterals :: TestTree
trailingSpaceAfterStringLiterals =
    Test.Tasty.HUnit.testCase "Trailing space after string literals" (do
        -- Verify that string literals parse correctly with trailing space
        -- (Yes, I did get this wrong at some point)
        _ <- Util.code "(''\nABC'' ++ \"DEF\" )"
        return () )

hasTypeErrorWithMessage ::  Text -> [ Text] -> IO ()
hasTypeErrorWithMessage dhallCode expectedErrorMessageSubstrings = do
            let handler :: Dhall.TypeCheck.TypeError Src Void -> IO (Bool, Maybe (Dhall.TypeCheck.TypeMessage Src Void))
                handler e = return (True, Just (typeMessage e))

            let typeCheck = do
                    _ <- Util.code dhallCode
                    return (False, Nothing)
            
            (b, Just actualTypeError) <- Control.Exception.handle handler typeCheck
            Test.Tasty.HUnit.assertBool ("The expression " <> Text.unpack dhallCode <> " should not type-check") b
            let message = prettyTypeMessage actualTypeError
            -- Concatenate the short and the long doc strings in an ErrorMessages structure:
            let concatenateAllDocStrings m = Pretty.vsep [short m, long m]
            -- Whether `message` has `text` as a substring:    
            let haveSubstring :: Text -> Bool
                haveSubstring text = Text.isInfixOf text (renderStrict (Pretty.layoutPretty Pretty.defaultLayoutOptions (concatenateAllDocStrings message)))
            let haveAllSubstrings = List.all haveSubstring expectedErrorMessageSubstrings

            Test.Tasty.HUnit.assertBool ("The error message for " <> Text.unpack dhallCode <> " should contain all expected substrings") haveAllSubstrings      
            return ()

-- Verify that each of the expressions fails to type-check and produces a diagnostic text that contains each of the given substrings.
combineTypes :: TestTree
combineTypes = Test.Tasty.HUnit.testCase "Combine / CombineTypes error messages" (do
        hasTypeErrorWithMessage     "{ a : Bool } ∧ { b = 0 }"                ["You can only combine a record with another record", "∧", "↳ { a : Bool }", "which is not a record"]
        hasTypeErrorWithMessage     "{ a : Bool } ∧ { a : Natural }"          ["Field type collision on: a", "{ x : A } ∧ { y : B }"]
        hasTypeErrorWithMessage     "{ a = 0 } ∧ { b : Natural }"             ["You can only combine a record with another record", "∧", "↳ { b : Natural }", "which is not a record"]
        hasTypeErrorWithMessage     "{ a = 0 } ∧ { a : Natural }"             ["You can only combine a record with another record", "∧", "↳ { a : Natural }", "which is not a record"]
        hasTypeErrorWithMessage     "{ a : Bool } ∧ { a = 0 }"                ["You can only combine a record with another record", "∧", "↳ { a : Bool }", "which is not a record"]
        hasTypeErrorWithMessage     "{ a : Bool } ∧ { b = 0 }"                ["You can only combine a record with another record", "∧", "↳ { a : Bool }", "which is not a record"]
        hasTypeErrorWithMessage     "{ a : Bool } ∧  0"                       ["You can only combine two records or two record types", "↳ { a : Bool }", "↳ 0", "↳ Natural", "↳ Type", "At least one of these arguments is neither a record type nor a record."]
        hasTypeErrorWithMessage     "{ a = 0 } ∧  0"                          ["You can only combine a record with another record", "∧", "↳ 0", "↳ Natural", "which is not a record"]
        hasTypeErrorWithMessage     "0 ∧ { a : Bool }"                        ["You can only combine two records or two record types", "↳ { a : Bool }", "↳ 0", "↳ Natural", "↳ Type", "At least one of these arguments is neither a record type nor a record."]
        hasTypeErrorWithMessage     "0 ∧ { a = 0 }"                           ["You can only combine a record with another record", "∧", "↳ 0", "↳ Natural", "which is not a record"]
        hasTypeErrorWithMessage     "0 ∧ 0"                                   ["You can only combine two records or two record types", "↳ 0", "↳ Natural", "At least one of these arguments is neither a record type nor a record."]
        hasTypeErrorWithMessage     "{ a : Bool } ⩓ { a : Natural }"          ["Field type collision on: a", "{ x : A } ⩓ { y : B }"]
        hasTypeErrorWithMessage     "{ a = { b : Bool }, a = { c = 0 } }"     ["Invalid duplicate field: a", "↳ { b : Bool }", "↳ Type", "which is not a record type"]
        hasTypeErrorWithMessage     "{ a = { c = 0 }, a = { b : Bool } }"     ["Invalid duplicate field: a", "↳ { b : Bool }", "↳ Type", "which is not a record type"]
        hasTypeErrorWithMessage     "{ a = { b = 0 }, a = { b = 1 } }"        ["Duplicate field cannot be merged: a.b", "which collided on the following path:", "↳ a.b"]
        hasTypeErrorWithMessage     "{ a = { b : Bool }, a = { b : Text } }"  ["Field type collision on: b", "↳ b", "{ x : A } ∧ { y : B }"]
        hasTypeErrorWithMessage     "{ a : Bool } ⩓ { a : Natural }"          ["Field type collision on: a", "↳ a", "{ x : A } ⩓ { y : B }"]
        hasTypeErrorWithMessage     "{ a : Bool } ⩓ { a = 0 }"                ["requires arguments that are record types", "⩓", "↳ { a = 0 }", "not a record type literal"]
    )                       
