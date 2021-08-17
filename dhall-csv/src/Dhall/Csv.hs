{-#LANGUAGE OverloadedStrings#-}

{-| This library exports two functions: `dhallToCsv` and `codeToValue`.
    The former converts a Dhall expression (with imports resolved already) into a
    sequence of CSV `NamedRecord`s (from the @cassava@ library) while the latter
    converts a `Text` containing Dhall code into a list of CSV `NamedRecord`s.

    Not all Dhall expressions can be converted to CSV since CSV is not a
    programming language.  The only things you can convert are @List@s of
    records where each field is one of the following types:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@ values
    * @Optional@ (of valid field types)
    * unions (of empty alternatives or valid record field types)

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
> exampleOptional
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

import Control.Exception            (Exception, throwIO, displayException)
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
data CompileError
    = Unsupported (Expr Void Void)
    | NotAList (Expr Void Void)
    | NotARecord (Expr Void Void)
    | BareNone
    deriving (Show)

instance Exception CompileError where
    displayException (Unsupported e) =
        Data.Text.unpack $
            _ERROR <> ": Cannot translate record field to CSV                                \n\
            \                                                                                \n\
            \Explanation: Only the following types of record fields can be converted to CSV: \n\
            \                                                                                \n\
            \● ❰Bool❱                                                                        \n\
            \● ❰Natural❱                                                                     \n\
            \● ❰Integer❱                                                                     \n\
            \● ❰Double❱                                                                      \n\
            \● ❰Text❱                                                                        \n\
            \● ❰Optional t❱ (where ❰t❱ is a valid record field type)                         \n\
            \● Unions *                                                                      \n\
            \                                                                                \n\
            \* Unions can have empty alternatives or alternatives with valid                 \n\
            \  record field types                                                            \n\
            \                                                                                \n\
            \The following Dhall expression could not be translated to CSV:                  \n\
            \                                                                                \n\
            \" <> insert e

    displayException (NotAList e) =
        Data.Text.unpack $
            _ERROR <> ": Top level object must be of type ❰List❱                             \n\
            \                                                                                \n\
            \Explanation: To translate to CSV you must provide a list of records.            \n\
            \Other types can not be translated directly.                                     \n\
            \                                                                                \n\
            \Expected an expression of type List {...} but instead got the following         \n\
            \expression:                                                                     \n\
            \                                                                                \n\
            \" <> insert e

    displayException (NotARecord e) =
        Data.Text.unpack $
            _ERROR <> ": Elements of the top-level list must be records                      \n\
            \                                                                                \n\
            \Explanation: To translate to CSV you must provide a list of records.            \n\
            \Other types can not be translated directly.                                     \n\
            \                                                                                \n\
            \Expected a record but instead got the following expression:                     \n\
            \                                                                                \n\
            \" <> insert e

    displayException BareNone =
       Data.Text.unpack $
            _ERROR <> ": ❰None❱ is not valid on its own                                      \n\
            \                                                                                \n\
            \Explanation: The conversion to JSON/YAML does not accept ❰None❱ in isolation as \n\
            \a valid way to represent ❰null❱.  In Dhall, ❰None❱ is a function whose input is \n\
            \a type and whose output is an ❰Optional❱ of that type.                          \n\
            \                                                                                \n\
            \For example:                                                                    \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌─────────────────────────────────┐  ❰None❱ is a function whose result is   \n\
            \    │ None : ∀(a : Type) → Optional a │  an ❰Optional❱ value, but the function  \n\
            \    └─────────────────────────────────┘  itself is not a valid ❰Optional❱ value \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌─────────────────────────────────┐  ❰None Natural❱ is a valid ❰Optional❱   \n\
            \    │ None Natural : Optional Natural │  value (an absent ❰Natural❱ number in   \n\
            \    └─────────────────────────────────┘  this case)                             \n\
            \                                                                                \n\
            \                                                                                \n\
            \                                                                                \n\
            \The conversion to CSV only translates the fully applied form to empty string.   "

insert :: Pretty a => a -> Text
insert = Pretty.renderStrict . Dhall.Pretty.layout . Dhall.Util.insert

{-| Convert a Dhall expression (with resolved imports) to an
    sequence of CSV @NamedRecord@s.
-}
dhallToCsv
    :: Expr s Void
    -> Either CompileError (Seq NamedRecord)
dhallToCsv e0 = listConvert $ Core.normalize e0
  where
    listConvert :: Expr Void Void -> Either CompileError (Seq NamedRecord)
    listConvert (Core.ListLit _ a) = traverse recordConvert a
    listConvert e = Left $ NotAList e
    recordConvert :: Expr Void Void -> Either CompileError NamedRecord
    recordConvert (Core.RecordLit a) = do
        a' <- traverse (fieldConvert . Core.recordFieldValue) a
        return $ Data.Csv.toNamedRecord $ Dhall.Map.toMap a'
    recordConvert e = Left $ NotARecord e
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
    fieldConvert Core.None = Left BareNone
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
