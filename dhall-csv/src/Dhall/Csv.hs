{-#LANGUAGE OverloadedStrings#-}

module Dhall.Csv (
      hello
    , dhallToCsv
    , codeToValue
    ) where

import Control.Exception            (Exception, throwIO)
import Data.Csv                     (ToField (..))
import Data.Maybe                   (fromMaybe)
import Data.Sequence                (Seq)
import Data.Text                    (Text)
import Data.Text.Prettyprint.Doc    (Pretty)
import Data.Void                    (Void)
import Dhall.Core                   (Expr, DhallDouble (..))
import Dhall.Import                 (SemanticCacheMode (..))

import qualified Data.Csv
import qualified Data.Foldable
import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall.Core                            as Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Dhall.Util
import qualified System.FilePath

data CompileError = Unsupported (Expr Void Void)

instance Show CompileError where
    show (Unsupported e) =
        Data.Text.unpack $
            _ERROR <> ": Cannot translate to CSV                                             \n\
            \                                                                                \n\
            \Explanation: Only records of primitive values can be                            \n\
            \translated from Dhall to CSV.                                                   \n\
            \                                                                                \n\
            \The following Dhall expression could not be translated to CSV:                  \n\
            \                                                                                \n\
            \" <> insert e

instance Exception CompileError

_ERROR :: Text
_ERROR = Dhall.Util._ERROR

insert :: Pretty a => a -> Text
insert = Pretty.renderStrict . Dhall.Pretty.layout . Dhall.Util.insert

hello :: String
hello = "Hello Dhall to CSV!"

dhallToCsv
    :: Expr s Void
    -> Either CompileError (Seq Data.Csv.NamedRecord)
dhallToCsv e0 = listConvert $ Core.alphaNormalize $ Core.normalize e0
  where
    listConvert :: Expr Void Void -> Either CompileError (Seq Data.Csv.NamedRecord)
    listConvert (Core.ListLit _ a) = do
        a' <- traverse recordConvert a
        return a'
    listConvert e = Left $ Unsupported e
    recordConvert :: Expr Void Void -> Either CompileError Data.Csv.NamedRecord
    recordConvert (Core.RecordLit a) = do
        a' <- traverse (fieldConvert . Core.recordFieldValue) a
        return $ Data.Csv.toNamedRecord $ Dhall.Map.toMap a'
    recordConvert e = Left $ Unsupported e
    fieldConvert :: Expr Void Void -> Either CompileError Data.Csv.Field
    fieldConvert (Core.NaturalLit a) = return $ toField a
    fieldConvert (Core.IntegerLit a) = return $ toField a
    fieldConvert (Core.DoubleLit (DhallDouble a)) = return $ toField a
    fieldConvert (Core.TextLit (Core.Chunks [] a)) = return $ toField a
    fieldConvert (Core.App (Core.Field (Core.Union _) _) a) = fieldConvert a
    fieldConvert e = Left $ Unsupported e

codeToValue
    :: Maybe FilePath
    -> Text
    -> IO [Data.Csv.NamedRecord]
codeToValue mFilePath code = do
    parsedExpression <- Core.throws (Dhall.Parser.exprFromText (fromMaybe "(input)" mFilePath) code)

    let rootDirectory = case mFilePath of
            Nothing -> "."
            Just fp -> System.FilePath.takeDirectory fp

    resolvedExpression <- Dhall.Import.loadRelativeTo rootDirectory UseSemanticCache parsedExpression

    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    case dhallToCsv resolvedExpression of
        Left err -> throwIO err
        Right csv -> return $ Data.Foldable.toList csv
