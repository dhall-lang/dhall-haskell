{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE BangPatterns            #-}
module Backend.Dhall.Diagnostics( compilerDiagnostics
                                , defaultDiagnosticSource
                                ) where


{-|
This module is responsible for producing dhall compiler diagnostic (errors, warns, etc ...)
-}

import qualified Control.Exception
import qualified Dhall
import Dhall(rootDirectory, sourceName, defaultInputSettings, inputExprWithSettings)
import Dhall.Parser(ParseError(..), Src(..), SourcedException(..))
import qualified Dhall.Core
import qualified System.Exit
import qualified System.IO
import Lens.Family (LensLike', set, view)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError(..), X)

import Dhall.Binary(DecodingFailure(..))
import Dhall.Import(Imported(..), Cycle(..), ReferentiallyOpaque(..),
                     MissingFile, MissingEnvironmentVariable, MissingImports(..) )


import qualified Data.Text as T


import qualified Text.Megaparsec
import qualified Text.Megaparsec.Error
import Text.Show(ShowS)
import qualified Data.Set
import qualified System.FilePath

import Backend.Dhall.DhallErrors(simpleTypeMessage)

import Language.Haskell.LSP.Types(
      Diagnostic(..)
    , Range(..)
    , DiagnosticSeverity(..)
    , DiagnosticSource(..)
    , DiagnosticRelatedInformation(..)
    , Position(..)
    )

defaultDiagnosticSource :: DiagnosticSource
defaultDiagnosticSource = "dhall-lsp-server"

-- TODO: type errors span across whitespace after the expression
-- TODO: don't use show for import msgs (requires alternative typeclass)
-- TODO: file consisting with only comments shouldn't produce an error msg
compilerDiagnostics :: FilePath -> Text -> IO [Diagnostic]
compilerDiagnostics path txt = handle ast
  where
    (rootDir, bufferName) = System.FilePath.splitFileName path
    settings =  ( set rootDirectory rootDir
                . set sourceName bufferName) defaultInputSettings
    isEmpty = T.null $ T.strip txt
    ast =  if isEmpty 
           then pure []
           else [] <$ inputExprWithSettings  settings txt
    handle =   Control.Exception.handle allErrors
             . Control.Exception.handle decodingFailure
             . Control.Exception.handle missingImports
             . Control.Exception.handle parseErrors
             . Control.Exception.handle typeErrors
             
    
    allErrors e = do 
      let _ = e :: SomeException
          numLines = length $ T.lines txt
      pure [Diagnostic {
              _range = Range (Position 0 0) (Position numLines 0)
            , _severity = Just DsError
            , _source = Just defaultDiagnosticSource
            , _code = Nothing
            , _message = "Internal error has occurred: " <> (show e)
            , _relatedInformation = Nothing
            }]
    decodingFailure e = do
      let (CBORIsNotDhall term) = e
      pure [Diagnostic {
        _range = Range (Position 0 0) (Position 1 0)
      , _severity = Just DsError
      , _source = Just defaultDiagnosticSource
      , _code = Nothing
      , _message = "Cannot decode CBOR to Dhall " <> (show term)
      , _relatedInformation = Nothing
      }]
    parseErrors e = do
        let _ = e :: ParseError
            errors = errorBundleToDiagnostics $ unwrap e
        System.IO.hPrint System.IO.stderr errors
        pure $ errors
    missingImports (SourcedException src e) = do
      let _ = e :: MissingImports
      pure [Diagnostic {
             _range = sourceToRange src
           , _severity = Just DsError
           , _source = Just defaultDiagnosticSource
           , _code = Nothing
           , _message = removeAsciiColors $ show e
           , _relatedInformation = Nothing
           }]
    typeErrors e = do
      let _ = e :: TypeError Src X
          (TypeError ctx expr msg) = e
      pure [ Diagnostic {
        _range = getSourceRange e
      , _severity = Just DsError
      , _source = Just defaultDiagnosticSource
      , _code = Nothing
      , _message =  (simpleTypeMessage msg) 
      , _relatedInformation = Nothing
      }] 


removeAsciiColors :: Text -> Text
removeAsciiColors = T.replace "\ESC[1;31m" "" . T.replace "\ESC[0m" ""


getSourceRange :: TypeError Src X -> Range
getSourceRange (TypeError ctx expr msg) =  case expr of
                Dhall.Core.Note src _ -> sourceToRange src
                _        -> error  "Expected note"  -- FIXME: either justify this error or provide a default case
  where
    unPos = Text.Megaparsec.unPos

sourcePosToRange :: Text.Megaparsec.SourcePos -> Text.Megaparsec.SourcePos -> Range
sourcePosToRange  (Text.Megaparsec.SourcePos _ bl bc) (Text.Megaparsec.SourcePos _ el ec) =
    Range (Position (unPos bl - 1) (unPos bc - 1)) (Position (unPos el - 1) (unPos ec - 1))
  where
    unPos = Text.Megaparsec.unPos

sourceToRange :: Src -> Range
sourceToRange (Src start end _) = sourcePosToRange start end

---------------------- Megaparsec utils: ----------------------------------------

-- see Text.Megaparsec.Error::errorBundlePretty for reference
errorBundleToDiagnostics
  :: forall s e. ( Text.Megaparsec.Stream s
  , Text.Megaparsec.Error.ShowErrorComponent e
  )
  => Text.Megaparsec.ParseErrorBundle s e
  -> [Diagnostic]
errorBundleToDiagnostics  Text.Megaparsec.Error.ParseErrorBundle {..}  = 
    fst $ foldl' f ([], bundlePosState) bundleErrors
  where
    f :: forall s e. ( Text.Megaparsec.Stream s, Text.Megaparsec.Error.ShowErrorComponent e)
      => ([Diagnostic], Text.Megaparsec.PosState s)
      -> Text.Megaparsec.ParseError s e
      -> ([Diagnostic], Text.Megaparsec.PosState s)  
    f (r, !pst) e = (diagnostics:r, pst')
      where
        (epos, line, pst') = Text.Megaparsec.reachOffset (Text.Megaparsec.errorOffset e) pst
        errorText = Text.Megaparsec.Error.parseErrorTextPretty e
        lineNumber = (Text.Megaparsec.unPos . Text.Megaparsec.sourceLine) epos - 1
        startColumn = Text.Megaparsec.unPos (Text.Megaparsec.sourceColumn epos) - 1
        diagnostics = Diagnostic {
            _range = Range (Position lineNumber startColumn) (Position lineNumber endColumn)
          , _severity = Just DsError
          , _source = Just defaultDiagnosticSource
          , _code = Nothing
          , _message = T.pack errorText
          , _relatedInformation = Nothing
          }
        
        endColumn = startColumn + errorLength
         
        lineLength = length line
        errorLength =
          case e of
            Text.Megaparsec.TrivialError _ Nothing _ -> 1
            Text.Megaparsec.TrivialError _ (Just x) _ -> errorItemLength x
            Text.Megaparsec.FancyError _ xs ->
              Data.Set.foldl' (\a b -> max a (errorFancyLength b)) 1 xs

-- | Get length of the “pointer” to display under a given 'ErrorItem'.

errorItemLength :: Text.Megaparsec.ErrorItem t -> Int
errorItemLength = \case
  Text.Megaparsec.Tokens ts -> length ts
  _         -> 1

errorFancyLength :: Text.Megaparsec.ShowErrorComponent e => Text.Megaparsec.ErrorFancy e -> Int  
errorFancyLength = \case
  Text.Megaparsec.ErrorCustom a -> Text.Megaparsec.errorComponentLen a
  _             -> 1
