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
import Dhall.Parser(ParseError(..), Src(..))
import qualified Dhall.Core
import qualified System.Exit
import qualified System.IO
import Lens.Family (LensLike', set, view)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError(..), X)

import Dhall.Binary(DecodingFailure(..))
import Dhall.Import(Imported(..), Cycle(..), ReferentiallyOpaque(..),
                     MissingFile, MissingEnvironmentVariable, MissingImports )


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

-- FIXME: type errors span across whitespace after the expression
--  Dhall.Binary.DecodingFailure
--  Dhall.Import(Cycle, ReferentiallyOpaque, MissingFile, MissingEnvironmentVariable, MissingImports,
--   HashMismatch, CannotImportHTTPURL)
-- !FIXME: (aside) VSCode multiselection expand selects first world only
compilerDiagnostics :: FilePath -> Text -> Text -> IO [Diagnostic]
compilerDiagnostics path filePath txt = handle ast
  where
    -- bufferName = T.unpack $ last $ fromList $ T.split (=='/') filePath
    -- rootDir    = T.unpack $ T.intercalate "/" $ tail $ fromList $ T.split (=='/') filePath
    (rootDir, bufferName) = System.FilePath.splitFileName path
    settings =  ( set rootDirectory rootDir
                . set sourceName bufferName) defaultInputSettings
    isEmpty = T.null $ T.strip txt -- FIXME: file consisting with only comments shouldn't produce an error? handle upstream?
    ast =  if isEmpty 
           then pure []
           else [] <$ inputExprWithSettings  settings txt
    handle =   Control.Exception.handle allErrors
             . Control.Exception.handle decodingFailure
             . handleImportErrors txt
             . Control.Exception.handle parseErrors
             . Control.Exception.handle importErrors
             . Control.Exception.handle moduleErrors
             
    
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
    importErrors (Imported ps e) = do
      let _ = e :: TypeError Src X
          numLines = length $ T.lines txt
      System.IO.hPrint System.IO.stderr (show ps)
      pure [ Diagnostic {
               _range = Range (Position 0 0) (Position numLines 0) -- getSourceRange e
             , _severity = Just DsError
             , _source = Just defaultDiagnosticSource
             , _code = Nothing
             , _message =  ("import error: " <> (show e)) -- FIXME: simple show for import msgs
             , _relatedInformation = Nothing
             }]
    moduleErrors e = do
      let _ = e :: TypeError Src X
          (TypeError ctx expr msg) = e
      -- System.IO.hPrint System.IO.stderr txt    
      -- System.IO.hPrint System.IO.stderr e
      pure [ Diagnostic {
        _range = getSourceRange e
      , _severity = Just DsError
      , _source = Just defaultDiagnosticSource
      , _code = Nothing
      , _message =  (simpleTypeMessage msg) -- FIXME: using show for import msgs
      , _relatedInformation = Nothing
      }] 

-- ! FIXME: provide import errors source position (should be handled in the dhall project)
-- * Import Errors provide no source pos info, except import mode and ImportType (which contains actual url)
handleImportErrors :: Text -> IO [Diagnostic] -> IO [Diagnostic]
handleImportErrors txt =   Control.Exception.handle (importHandler @Cycle)
                     . Control.Exception.handle (importHandler @ReferentiallyOpaque)
                     . Control.Exception.handle (importHandler @MissingFile)
                     . Control.Exception.handle (importHandler @MissingEnvironmentVariable)
                     . Control.Exception.handle (importHandler @MissingImports)
                     
  where
    numLines = length $ T.lines txt
    importHandler:: forall e a. Exception e => (e -> IO [Diagnostic])
    importHandler e =
      pure [Diagnostic {
        _range = Range (Position 0 0) (Position numLines 0)
      , _severity = Just DsError
      , _source = Just defaultDiagnosticSource
      , _code = Nothing
      , _message = removeAsciiColors $ show e
      , _relatedInformation = Nothing
      }]
    
removeAsciiColors :: Text -> Text
removeAsciiColors = T.replace "\ESC[1;31m" "" . T.replace "\ESC[0m" ""
--  Dhall.Import(Cycle, ReferentiallyOpaque, MissingFile, MissingEnvironmentVariable, MissingImports,
--   HashMismatch, CannotImportHTTPURL)

getSourceRange :: TypeError Src X -> Range
getSourceRange (TypeError ctx expr msg) =  case expr of
                Dhall.Core.Note (Src (Text.Megaparsec.SourcePos _ bl bc) (Text.Megaparsec.SourcePos _ el ec) _) _ -> 
                  Range (Position (unPos bl - 1) (unPos bc - 1)) (Position (unPos el - 1) (unPos ec - 1))
                _        -> error  "expected note" -- $ Range (Position 0 0) (Position (negate 1) 0) -- FIXME: default case 
  where
    unPos = Text.Megaparsec.unPos




---------------------- Megaparsec utils: ----------------------------------------


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
-- errorBundlePretty
--   :: forall s e. ( Text.Megaparsec.Stream s
--                  , Text.Megaparsec.Error.ShowErrorComponent e
--                  )
--   => Text.Megaparsec.ParseErrorBundle s e -- ^ Parse error bundle to display
--   -> String               -- ^ Textual rendition of the bundle
-- errorBundlePretty Text.Megaparsec.Error.ParseErrorBundle {..} =
--   let (r, _) = foldl' f (id, bundlePosState) bundleErrors
--   in drop 1 (r "")
--   where
--     f :: (ShowS, Text.Megaparsec.PosState s)
--       -> Text.Megaparsec.ParseError s e
--       -> (ShowS, Text.Megaparsec.PosState s)
--     f (o, !pst) e = (o . (outChunk ++), pst')
--       where
--         (epos, sline, pst') = reachOffset (errorOffset e) pst
--         outChunk =
--           "\n" <> sourcePosPretty epos <> ":\n" <>
--           padding <> "|\n" <>
--           lineNumber <> " | " <> sline <> "\n" <>
--           padding <> "| " <> rpadding <> pointer <> "\n" <>
--           Text.Megaparsec.Error.parseErrorTextPretty e
--         lineNumber = (show . unPos . sourceLine) epos
--         padding = replicate (length lineNumber + 1) ' '
--         rpadding =
--           if pointerLen > 0
--             then replicate rpshift ' '
--             else ""
--         rpshift = unPos (sourceColumn epos) - 1
--         pointer = replicate pointerLen '^'
--         pointerLen =
--           if rpshift + elen > slineLen
--             then slineLen - rpshift + 1
--             else elen
--         slineLen = length sline
--         elen =
--           case e of
--             Text.Megaparsec.TrivialError _ Nothing _ -> 1
--             Text.Megaparsec.TrivialError _ (Just x) _ -> errorItemLength x
--             Text.Megaparsec.FancyError _ xs ->
--               Data.Set.foldl' (\a b -> max a (errorFancyLength b)) 1 xs
 