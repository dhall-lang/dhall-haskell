module Backend.Dhall.DiagnosticsSpec where

import Test.Tasty.Hspec


import Language.Haskell.LSP.Types(
      Diagnostic(..)
    , Range(..)
    , DiagnosticSeverity(..)
    , Position(..)
    )

import Data.Foldable (traverse_)
import Dhall.LSP.Handlers.Diagnostics (compilerDiagnostics)

import qualified Data.Text
import qualified Data.Text.IO

import qualified GHC.IO.Encoding
import qualified System.IO


spec_prelude :: Spec
spec_prelude = do
  
  runIO $ GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

  describe "Backend.Dhall.Diagnostics" $ do
   
    it "should produce an empty diagnostics for an empty file" $ do
      rs <- compilerDiagnostics "./empty.txt" ""
      rs `shouldBe` ([])

    it "should produce an empty diagnostics for a correct file (sanity check)" $ do
      traverse_ (\x -> x `shouldReturn` []) (getDiagnostics <$> successImports)

    it "should produce correct diagnostics for various missing imports" $ do 
      getDiagnostics "../dhall/dhall-lang/tests/import/failure/missing.dhall" `shouldReturn` 
        (mkDiagnostics "\nError: No valid imports" (0,0) (1,0))
      getDiagnostics "../dhall/dhall-lang/tests/import/failure/alternativeEnv.dhall" `shouldReturn` 
        (mkDiagnostics ("\nError: Failed to resolve imports. Error list:\n\n\n\8627 env:UNSET1 as Text\n\n" <> 
                        "Error: Missing environment variable\n\n\8627 UNSET1\n\n\n\8627 env:UNSET2\n\n" <>
                        "Error: Missing environment variable\n\n\8627 UNSET2\n\n\n\8627 env:UNSET3\n\n" <> 
                        "Error: Missing environment variable\n\n\8627 UNSET3\n") (0,0) (1,0))
      getDiagnostics "../dhall/dhall-lang/tests/import/failure/alternativeEnvMissing.dhall" `shouldReturn` 
        (mkDiagnostics "\n\8627 env:UNSET\n\nError: Missing environment variable\n\n\8627 UNSET" (0,0) (1,0))
      getDiagnostics "../dhall/dhall-lang/tests/import/failure/cycle.dhall" `shouldReturn` 
        (mkDiagnostics ("\n\8627 ./../dhall/dhall-lang/tests/import/data/cycle.dhall\n" <> 
                       "  \8627 ./../dhall/dhall-lang/tests/import/failure/cycle.dhall\n\n" <> 
                       "Cyclic import: ../data/cycle.dhall") (0,0) (1,0))
      getDiagnostics "../dhall/dhall-lang/tests/import/failure/referentiallyInsane.dhall" `shouldReturn` 
        (mkDiagnostics ("\n\8627 https://raw.githubusercontent.com/dhall-lang/dhall-lang/master" <> 
                        "/tests/import/data/referentiallyOpaque.dhall\n\n" <> 
                        "Referentially opaque import: env:HOME as Text") (19,0) (20,0))

    it "should produce correct diagnostic for various parser errors" $ do
      getDiagnostics "../dhall/dhall-lang/tests/parser/failure/boundBuiltins.dhall" `shouldReturn` 
          (mkDiagnostics "expecting label or whitespace\n" (5,8) (5,9))
      getDiagnostics "../dhall/dhall-lang/tests/parser/failure/doubleBoundsNeg.dhall" `shouldReturn` 
          (mkDiagnostics "double out of bounds\n" (0,0) (0,1))
      getDiagnostics "../dhall/dhall-lang/tests/parser/failure/doubleBoundsPos.dhall" `shouldReturn` 
          (mkDiagnostics "double out of bounds\n" (0,0) (0,1))
      getDiagnostics "../dhall/dhall-lang/tests/parser/failure/importAccess.dhall" `shouldReturn` 
          (mkDiagnostics ("unexpected '.'\nexpecting \"!=\", \"&&\", \"++\", \"->\", \"//\", \"//\\\\\", " <>
                          "\"/\\\", \"==\", \"as\", \"sha256:\", \"||\", \"\8594\", \"\8743\", \"\10835\", " <>
                          "\"\11005\", '#', '*', '+', ':', '?', end of input, or whitespace\n") (0,13) (0,14))
      getDiagnostics "../dhall/dhall-lang/tests/parser/failure/incompleteIf.dhall" `shouldReturn` 
          (mkDiagnostics "unexpected end of input\nexpecting expression or whitespace\n" (10,0) (10,1))
      getDiagnostics "../dhall/dhall-lang/tests/parser/failure/mandatoryNewline.dhall" `shouldReturn` 
          (mkDiagnostics "unexpected \"AB\"\nexpecting crlf newline or newline\n" (1,2) (1,4))
      
      {--
      getDiagnostics "../dhall/dhall-lang/tests/parser/failure/missingSpace.dhall" `shouldReturn` 
          (mkDiagnostics ("\n\8627 ./../dhall/dhall-lang/tests/parser/failure/example.dhall\n\n" <> 
                          "Error: Missing file /Users/edevi86/lab/haskell/dhall-haskell/dhall-l" <> 
                          "sp-server/../dhall/dhall-lang/tests/parser/failure/example.dhall") (1,0) (1,15))
       --}
    it "should produce correct diagnostic for various typecheck errors" $ do
      getDiagnostics "../dhall/dhall-lang/tests/typecheck/failure/combineMixedRecords.dhall" `shouldReturn` 
          (mkDiagnostics "Record mismatch" (0,0) (1,0))
    it "should produce correct diagnostic for various typecheck errors" $ do
          getDiagnostics "../dhall/dhall-lang/tests/typecheck/failure/duplicateFields.dhall" `shouldReturn` 
              (mkDiagnostics "duplicate field: a\n" (0,15) (0,16))
    it "should produce correct diagnostic for various typecheck errors" $ do
          getDiagnostics "../dhall/dhall-lang/tests/typecheck/failure/hurkensParadox.dhall" `shouldReturn` 
              (mkDiagnostics "\10096Sort\10097 has no type, kind, or sort" (6,4) (49,0))
    it "should produce correct diagnostic for various typecheck errors" $ do
          getDiagnostics "../dhall/dhall-lang/tests/typecheck/failure/mixedUnions.dhall" `shouldReturn` 
              (mkDiagnostics "Alternative annotation mismatch" (0,0) (1,0))
    it "should produce correct diagnostic for various typecheck errors" $ do
          getDiagnostics "../dhall/dhall-lang/tests/typecheck/failure/preferMixedRecords.dhall" `shouldReturn` 
              (mkDiagnostics "Record mismatch" (0,0) (1,0))


getDiagnostics :: FilePath -> IO [Diagnostic]
getDiagnostics path = do
  text <- Data.Text.IO.readFile path
  compilerDiagnostics path text

successImports :: [String]
successImports = [ "../dhall/dhall-lang/tests/import/success/alternativeEnvNaturalA.dhall"
                 , "../dhall/dhall-lang/tests/import/success/alternativeEnvSimpleA.dhall"
                 , "../dhall/dhall-lang/tests/import/success/alternativeNaturalA.dhall"
                 , "../dhall/dhall-lang/tests/import/success/asTextA.dhall"
                 , "../dhall/dhall-lang/tests/import/success/fieldOrderA.dhall"
                 , "../dhall/dhall-lang/tests/import/success/alternativeEnvNaturalB.dhall"
                 , "../dhall/dhall-lang/tests/import/success/alternativeEnvSimpleB.dhall"
                 , "../dhall/dhall-lang/tests/import/success/alternativeNaturalB.dhall"
                 , "../dhall/dhall-lang/tests/import/success/asTextB.dhall"
                 , "../dhall/dhall-lang/tests/import/success/fieldOrderB.dhall"]  

mkDiagnostics :: Data.Text.Text
              -> (Int, Int)
              -> (Int, Int)
              -> [Diagnostic]
mkDiagnostics msg (sl, sc) (el, ec) = [
  Diagnostic {
  _range = Range {
             _start = Position {_line = sl, _character = sc} 
           , _end = Position {_line = el, _character = ec}
           }
  , _severity = Just DsError
  , _code = Nothing
  , _source = Just "dhall-lsp-server"
  , _message = msg
  , _relatedInformation = Nothing
  }]
