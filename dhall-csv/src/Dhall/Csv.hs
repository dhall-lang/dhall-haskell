{-#LANGUAGE OverloadedStrings#-}

{-| This library exports two functions: `dhallToCsv` and `codeToValue`.
    The former converts a Dhall Expression (with imports resolved already) into a
    sequence of CSV `NamedRecord` (from the @cassava@ library) while the latter
    converts a Text containing Dhall code into a list of CSV `NamedRecord` wrapped
    in the `IO` Monad (because of the import resolution).

    Not all Dhall expressions can be converted to CSV since CSV is not a
    programming language.  The only things you can convert are @List@s of
    records where each field is one of the following types:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@ values
    * @Optional@ values (one of these types)
    * unions (of these types)

    Dhall @Bool@s translate to either `"true"` or `"false"` in all lowercase letters:

> $ dhall-to-csv <<< '[{ exampleBool = True }]'
> exampleBool
> true
> $ dhall-to-csv <<< '[{ exampleBool = False }]'
> exampleBool
> false

    Dhall numbers translate to their string representations:

> $ dhall-to-csv <<< '[{ exampleInteger = +2 }]'
> exampleInteger
> 2
> $ dhall-to-csv <<< '[{ exampleNatural = 2 }]'
> exampleNatural
> 2
> $ dhall-to-csv <<< '[{ exampleDouble = 2.3 }]'
> exampleDouble
> 2.3

    Dhall @Text@ translates directly to CSV. Special CSV characters
    are enclosed by double quotes:

> $ dhall-to-csv <<< '[{ exampleText = "ABC" }]'
> exampleText
> ABC
> $ dhall-to-csv <<< '[{ exampleText = "ABC,ABC" }]'
> exampleText
> "ABC,ABC"

    Dhall @Optional@ values translate to the empty string if absent and the unwrapped
    value otherwise:

> $ dhall-to-csv <<< '[{ exampleOptional = None Natural }]'
> exampleInt,exampleOptional
>
> $ dhall-to-csv <<< '[{ exampleOptional = Some 1 }]'
> exampleOptional
> 1

    Dhall unions translate to the wrapped value or the name of the field
    (in case it is an empty field):

> $ dhall-to-csv <<< "[{ exampleUnion = < Left | Right : Natural>.Left }]"
> exampleUnion
> Left
> $ dhall-to-csv <<< "[{ exampleUnion = < Left | Right : Natural>.Right 2 }]"
> exampleUnion
> 2

    Also, all Dhall expressions are normalized before translation to CSV:

> $ dhall-to-csv <<< "[{ equailty = True == False }]"
> equality
> false
-}

module Dhall.Csv (
      dhallToCsv
    , codeToValue

    -- * Exceptions
    , CompileError
    ) where

import Control.Exception            (Exception, throwIO)
import Data.Csv                     (NamedRecord, ToField (..))
import Data.Maybe                   (fromMaybe)
import Data.Sequence                (Seq)
import Data.Text                    (Text)
import Data.Text.Prettyprint.Doc    (Pretty)
import Data.Void                    (Void)
import Dhall.Core                   (Expr, DhallDouble (..))
import Dhall.Import                 (SemanticCacheMode (..))
import Dhall.Util                   (_ERROR)

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

{-| This is the exception type for errors that can arise when converting from
    Dhall to CSV.

    It contains information on the specific cases that might
    fail to give a better insight.
-}
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

insert :: Pretty a => a -> Text
insert = Pretty.renderStrict . Dhall.Pretty.layout . Dhall.Util.insert

{-| Convert a Dhall Expression (with resolved imports) to an equivalent
    sequence of CSV @NamedRecord@s.
-}
dhallToCsv
    :: Expr s Void
    -> Either CompileError (Seq NamedRecord)
dhallToCsv e0 = listConvert $ Core.normalize e0
  where
    listConvert :: Expr Void Void -> Either CompileError (Seq NamedRecord)
    listConvert (Core.ListLit _ a) = traverse recordConvert a
    listConvert e = Left $ Unsupported e
    recordConvert :: Expr Void Void -> Either CompileError NamedRecord
    recordConvert (Core.RecordLit a) = do
        a' <- traverse (fieldConvert . Core.recordFieldValue) a
        return $ Data.Csv.toNamedRecord $ Dhall.Map.toMap a'
    recordConvert e = Left $ Unsupported e
    fieldConvert :: Expr Void Void -> Either CompileError Data.Csv.Field
    fieldConvert (Core.BoolLit True) = return $ toField ("true" :: Text)
    fieldConvert (Core.BoolLit False) = return $ toField ("false" :: Text)
    fieldConvert (Core.NaturalLit a) = return $ toField a
    fieldConvert (Core.IntegerLit a) = return $ toField a
    fieldConvert (Core.DoubleLit (DhallDouble a)) = return $ toField a
    fieldConvert (Core.TextLit (Core.Chunks [] a)) = return $ toField a
    fieldConvert (Core.App (Core.Field (Core.Union _) _) a) = fieldConvert a
    fieldConvert (Core.Field (Core.Union _) (Core.FieldSelection _ k _)) = return $ toField k
    fieldConvert (Core.Some e) = fieldConvert e
    fieldConvert (Core.App Core.None _) = return $ toField ("" :: Text)
    fieldConvert e = Left $ Unsupported e

{-| Convert a @Text@ with Dhall code to a list of @NamedRecord@s.
-}
codeToValue
    :: Maybe FilePath
    -> Text
    -> IO [NamedRecord]
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
