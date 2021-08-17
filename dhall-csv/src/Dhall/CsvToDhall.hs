{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

{-| Convert CSV data to Dhall providing an expected Dhall type necessary
    to know which type will be interpreted.

    The translation process will produce a Dhall Expression where
    its type is a @List@ of records and the type of each field of the
    records is one of the following:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@s
    * @Optional@s (of valid field types)
    * unions (of empty alternatives or valid record field types)

    It is exactly the same as @dhall-to-csv@ supported input types.

    You can use this code as a library (this module) or as an executable
    named @csv-to-dhall@, which is used in the examples below.

    For now, @csv-to-dhall@ does not support type inference so you must
    always specify the Dhall type you expect.

> $ cat example.csv
> example
> 1
> $ csv-to-dhall 'List { example : Integer }' < example.csv
> [{ example = +1 }]

    When using the @csv-to-dhall@ executable you can specify that the CSV
    you want to translate does not have a header with the flag `--no-header`.
    In this case the resulting record fields will be named `_1`, `_2`, ...
    in the same order they where in the input CSV. You must still provide the
    expected Dhall type taking this into consideration.

> $ cat no-header-example.csv
> 1,3.14,Hello
> -1,2.68,Goodbye
> $ csv-to-dhall --no-header 'List { _1 : Integer, _2 : Double, _3 : Text } < no-header-example.csv
> [ { _1 = +1, _2 = 3.14, _3 = "Hello" }
> , { _1 = -1, _2 = 2.68, _3 = "Goodbye" }
> ]

== Primitive types

    Strings 'true' and 'false' can translate to Dhall @Bool@s

> $ cat example.csv
> exampleBool
> true
> false
> $ csv-to-dhall 'List { exampleBool : Bool }' < example.csv
> [ { exampleBool = True }, { exampleBool = False } ]

    Numeric strings can translate to Dhall numbers:

> $ cat example.csv
> exampleNatural,exampleInt,exampleDouble
> 1,2,3
> 0,-2,3.14
> 0,+2,-3.14
> $ csv-to-dhall 'List { exampleNatural : Natural, exampleInt : Integer, exampleDouble : Double }' < example.csv
> [ { exampleNatural = 1, exampleInt = +2, exampleDouble = 3.0 }
> , { exampleNatural = 0, exampleInt = -2, exampleDouble = 3.14 }
> , { exampleNatural = 0, exampleInt = +2, exampleDouble = -3.14 }
> ]

    Every CSV Field can translate directly to Dhall @Text@:

> $ cat example.csv
> exampleText
> Hello
> false
>
> ","
> $ csv-to-dhall 'List { exampleText : Text }' < example.csv
> [ { exampleText = "Hello" }
> , { exampleText = "false" }
> , { exampleText = "" }
> , { exampleText = "," }
> ]

== Unions and Optionals

    By default, when a union is expected, the first alternative that
    matches the CSV field is chosen. With the `--unions-strict` flag
    one can make sure that only one alternative matches. With the
    `--unions-none` unions are not allowed.

    An union alternative matches a CSV field if

    * It's an empty alternative and the name is the same as the text in the CSV field.
    * It's a non-empty alternative and the CSV field can be converted to the underlying type.

> $ cat example.csv
> exampleUnion
> Hello
> 1
> 1.11
> $ csv-to-dhall 'List { exampleUnion : <Hello | Nat : Natural | Dob : Double> }' < example.csv
> [ { exampleUnion = <Hello | Nat : Natural | Dob : Double>.Hello }
> , { exampleUnion = <Hello | Nat : Natural | Dob : Double>.Nat 1 }
> , { exampleUnion = <Hello | Nat : Natural | Dob : Double>.Dob 1.11 }
> ]

    Optional values can be either missing or have the expected value.
    The missing value is represented by the empty string.
    If a field's expected value is an Optional and the field is not
    in the CSV, then all the values will be None.

> $ cat example.csv
> exampleOptional
> 1
>
> 3
> $ csv-to-dhall 'List { exampleOptional : Optional Natural, exampleMissing : Optional Natural }' < example.csv
> [ { exampleOptional = Some 1, exampleMissing = None Natural }
> , { exampleOptional = None Natural, exampleMissing = None Natural }
> , { exampleOptional = Some 3, exampleMissing = None Natural }
> ]

-}


module Dhall.CsvToDhall (
    -- * CSV to Dhall
      dhallFromCsv
    , parseConversion
    , defaultConversion
    , resolveSchemaExpr
    , typeCheckSchemaExpr
    , Conversion(..)

    -- * Exceptions
    , CompileError(..)
    ) where

import Control.Applicative          ((<|>))
import Control.Exception            (Exception, throwIO)
import Control.Monad.Catch          (MonadCatch, throwM)
import Data.Csv                     (NamedRecord)
import Data.Either                  (lefts, rights)
import Data.Either.Combinators      (mapRight)
import Data.Foldable                (toList)
import Data.List                    ((\\))
import Data.Text                    (Text)
import Data.Text.Encoding           (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Text.Encoding.Error     (UnicodeException)
import Data.Text.Prettyprint.Doc    (Pretty)
import Data.Text.Read               (decimal, double, signed)
import Data.Void                    (Void)
import Dhall.Core                   (Expr)
import Dhall.Src                    (Src)
import Dhall.Util                   (_ERROR)
import Options.Applicative          (Parser)

import qualified Data.Csv
import qualified Data.HashMap.Strict                    as HashMap
import qualified Data.Sequence                          as Sequence
import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc.Render.Text  as Pretty
import qualified Dhall.Core                             as Core
import qualified Dhall.Import
import qualified Dhall.Map                              as Map
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck                        as TypeCheck
import qualified Dhall.Util
import qualified Options.Applicative                    as O

-- ----------
-- Conversion
-- ----------

-- | CSV-to-dhall translation options
data Conversion = Conversion
    { strictRecs :: Bool
    , unions     :: UnionConv
    } deriving Show

data UnionConv = UFirst | UNone | UStrict deriving (Show, Read, Eq)

-- | Default conversion options
defaultConversion :: Conversion
defaultConversion = Conversion
    { strictRecs     = False
    , unions         = UFirst
    }

-- ---------------
-- Command options
-- ---------------

-- | Standard parser for options related to the conversion method
parseConversion :: Parser Conversion
parseConversion = Conversion <$> parseStrict
                             <*> parseUnion
  where
    parseStrict =
            O.flag' True
            (  O.long "records-strict"
            <> O.help "Fail if any CSV fields are missing from the expected Dhall type"
            )
        <|> O.flag' False
            (  O.long "records-loose"
            <> O.help "Tolerate CSV fields not present within the expected Dhall type"
            )
        <|> pure True


-- | Parser for command options related to treating union types
parseUnion :: Parser UnionConv
parseUnion =
        uFirst
    <|> uNone
    <|> uStrict
    <|> pure UFirst -- defaulting to UFirst
  where
    uFirst  =  O.flag' UFirst
            (  O.long "unions-first"
            <> O.help "The first value with the matching type (successfully parsed all the way down the tree) is accepted, even if not the only possible match. (DEFAULT)"
            )
    uNone   =  O.flag' UNone
            (  O.long "unions-none"
            <> O.help "Unions not allowed"
            )
    uStrict =  O.flag' UStrict
            (  O.long "unions-strict"
            <> O.help "Error if more than one union values match the type (and parse successfully)"
            )

type ExprX = Expr Src Void

-- | Parse schema code and resolve imports
resolveSchemaExpr :: Text  -- ^ type code (schema)
                  -> IO ExprX
resolveSchemaExpr code = do
    parsedExpression <-
      case Dhall.Parser.exprFromText "\n\ESC[1;31m(schema)\ESC[0m" code of
        Left  err              -> throwIO err
        Right parsedExpression -> return parsedExpression
    Dhall.Import.load parsedExpression

-- | Check that the Dhall type expression actually has type 'Type'
typeCheckSchemaExpr :: (Exception e, MonadCatch m)
                    => (CompileError -> e) -> ExprX -> m ExprX
typeCheckSchemaExpr compileException expr =
  case TypeCheck.typeOf expr of -- check if the expression has type
    Left  err -> throwM . compileException $ TypeError err
    Right t   -> case t of -- check if the expression has type Type
      Core.Const Core.Type -> return expr
      _              -> throwM . compileException $ BadDhallType t expr


{-| Convert a list of CSV @NameRecord@ to a Dhall expression given the expected Dhall Type
    of the output.
-}
dhallFromCsv :: Conversion -> ExprX -> [NamedRecord] -> Either CompileError ExprX
dhallFromCsv Conversion{..} typeExpr = listConvert (Core.normalize typeExpr)
  where
    listConvert :: ExprX -> [NamedRecord] -> Either CompileError ExprX
    listConvert (Core.App Core.List recordType@(Core.Record _)) [] = return $ Core.ListLit (Just recordType) Sequence.empty
    listConvert (Core.App Core.List recordType) [] = Left $ NotARecord recordType
    listConvert (Core.App Core.List recordType) csv = do
        a <- traverse (recordConvert recordType) csv
        return $ Core.ListLit Nothing $ Sequence.fromList a
    listConvert e _ = Left $ NotAList e

    recordConvert :: ExprX -> NamedRecord -> Either CompileError ExprX
    recordConvert (Core.Record record) csvRecord
        | badKeys <- lefts (map decodeUtf8' (HashMap.keys csvRecord))
        , not (null badKeys)
        = Left $ UnicodeError (head badKeys) -- Only report first key that failed to be decoded
        | extraKeys <- (map decodeUtf8 $ HashMap.keys csvRecord) \\ Map.keys record
        , strictRecs && not (null extraKeys)
        = Left $ UnhandledKeys extraKeys
        | otherwise
        = do
            let f k v = fieldConvert k (Core.recordFieldValue v) (HashMap.lookup (encodeUtf8 k) csvRecord)
            a <- Map.traverseWithKey (\k v -> mapRight Core.makeRecordField (f k v)) record
            return $ Core.RecordLit a
    recordConvert e _ = Left $ NotARecord e

    fieldConvert :: Text -> ExprX -> Maybe Data.Csv.Field -> Either CompileError ExprX
    -- Unions
    fieldConvert recordKey t@(Core.Union tm) maybeField = do
        let f unionKey Nothing =
                case maybeField of
                    Nothing -> Left $ MissingKey recordKey
                    Just field ->
                        case decodeUtf8' field of
                            Left err -> Left $ UnicodeError err
                            Right _field ->
                                if _field == unionKey
                                then Right $ Core.Field t $ Core.makeFieldSelection unionKey
                                else Left $ Mismatch t _field recordKey
            f unionKey (Just _type) = do
                expression <- fieldConvert recordKey _type maybeField
                return (Core.App (Core.Field t $ Core.makeFieldSelection unionKey) expression)

        case (unions, rights (toList (Map.mapWithKey f tm)), maybeField) of
            (UNone  , _         , _         ) -> Left $ ContainsUnion t
            (UStrict, xs@(_:_:_), Just field) -> Left $ UndecidableUnion t (decodeUtf8 field) recordKey xs
            (UStrict, xs@(_:_:_), Nothing   ) -> Left $ UndecidableMissingUnion t recordKey xs
            (_      , []        , Just field) -> Left $ Mismatch t (decodeUtf8 field) recordKey
            (_      , []        , Nothing   ) -> Left $ MissingKey recordKey
            (UFirst , x:_       , _         ) -> Right $ x
            (UStrict, [x]       , _         ) -> Right $ x

    -- Missing Optionals
    fieldConvert _ (Core.App Core.Optional t) Nothing = return $ Core.App Core.None t

    -- Missing fields
    fieldConvert key _ Nothing = Left $ MissingKey key

    -- Bools
    fieldConvert key Core.Bool (Just field) =
        case decodeUtf8' field of
            Left err -> Left $ UnicodeError err
            Right _field ->
                case _field of
                    "true"  -> Right (Core.BoolLit True)
                    "false" -> Right (Core.BoolLit False)
                    _ -> Left $ Mismatch Core.Bool _field key

    -- Naturals
    fieldConvert key Core.Natural (Just field) =
        case decodeUtf8' field of
            Left err -> Left $ UnicodeError err
            Right _field ->
                case decimal _field of
                    Right (v, "") -> Right $ Core.NaturalLit v
                    _ -> Left $ Mismatch Core.Natural _field key

    -- Integers
    fieldConvert key Core.Integer (Just field) =
        case decodeUtf8' field of
            Left err -> Left $ UnicodeError err
            Right _field ->
                case (signed decimal) _field of
                    Right (v, "") -> Right $ Core.IntegerLit v
                    _ -> Left $ Mismatch Core.Integer _field key

    -- Doubles
    fieldConvert _ Core.Double (Just field) =
        case decodeUtf8' field of
            Left err -> Left $ UnicodeError err
            Right _field ->
                case double _field of
                    Right (v, "") -> Right $ Core.DoubleLit $ Core.DhallDouble v
                    _ -> Right $ Core.DoubleLit $ Core.DhallDouble (read "NaN")

    -- Text
    fieldConvert _ Core.Text (Just field) =
        case decodeUtf8' field of
            Left err -> Left $ UnicodeError err
            Right _field -> return $ Core.TextLit $ Core.Chunks [] $ _field

    -- Optionals null
    fieldConvert _ (Core.App Core.Optional t) (Just "") = return $ Core.App Core.None t

    -- Optionals
    fieldConvert key (Core.App Core.Optional t) maybeField = do
        expression <- fieldConvert key t maybeField
        return $ Core.Some expression

    fieldConvert _ t _ = Left $ Unsupported t


{-| This is the exception type for errors that can arise when converting from
    CSV to Dhall.

    It contains information on the specific cases that might
    fail to give a better insight.
-}
data CompileError
    = Unsupported ExprX
    | NotAList ExprX
    | NotARecord ExprX
    | TypeError (TypeCheck.TypeError Src Void)
    | BadDhallType
        ExprX -- Expression type
        ExprX -- Whole expression
    | MissingKey Text
    | UnhandledKeys [Text] -- Keys in CSV but not in schema
    | Mismatch
        ExprX           -- Expected Dhall Type
        Text            -- Actual field
        Text            -- Record key
    | ContainsUnion ExprX
    | UndecidableUnion
        ExprX           -- Expected Type
        Text            -- CSV Field
        Text            -- Record Key
        [ExprX]         -- Multiple conversions
    | UndecidableMissingUnion
        ExprX           -- Expected Type
        Text            -- Record Key
        [ExprX]         -- Multiple Conversions
    | UnicodeError UnicodeException

instance Show CompileError where
    show (Unsupported e) =
        Data.Text.unpack $
            _ERROR <> ": Invalid record field type                                           \n\
            \                                                                                \n\
            \Explanation: Only the following types of record fields are valid:               \n\
            \                                                                                \n\
            \● ❰Bool❱                                                                        \n\
            \● ❰Natural❱                                                                     \n\
            \● ❰Integer❱                                                                     \n\
            \● ❰Double❱                                                                      \n\
            \● ❰Text❱                                                                        \n\
            \● ❰Optional❱ tp (where tp is a valid record field type)                         \n\
            \● Unions *                                                                      \n\
            \                                                                                \n\
            \* Unions can have empty alternatives or alternatives with valid                 \n\
            \  record field types                                                            \n\
            \                                                                                \n\
            \Expected one of the previous types but instead got:                             \n\
            \                                                                                \n\
            \" <> insert e

    show (NotAList e) =
        Data.Text.unpack $
            _ERROR <> ": Top level object must be of type ❰List❱                             \n\
            \                                                                                \n\
            \Explanation: Translation from CSV only returns ❰List❱s of records.              \n\
            \Other types can not be translated.                                              \n\
            \                                                                                \n\
            \Expected type List {...} but instead got the following type:                    \n\
            \                                                                                \n\
            \" <> insert e

    show (NotARecord e) =
        Data.Text.unpack $
            _ERROR <> ": Elements of the top-level list must be records                      \n\
            \                                                                                \n\
            \Explanation: Translation from CSV only returns ❰List❱s of records.              \n\
            \Other types can not be translated.                                              \n\
            \                                                                                \n\
            \Expected a record type but instead got the following type:                      \n\
            \                                                                                \n\
            \" <> insert e

    show (TypeError e) = show e

    show (BadDhallType t e) =
        Data.Text.unpack $
            _ERROR <> ": Schema expression parsed successfully but has wrong Dhall type.     \n\
            \                                                                                \n\
            \Expected Dhall type: Type                                                       \n\
            \                                                                                \n\
            \Actual Dhall type:                                                              \n\
            \" <> insert t <>
            "                                                                                \n\
            \                                                                                \n\
            \Parsed Expression:                                                              \n\
            \" <> insert e

    show (MissingKey key) =
        Data.Text.unpack $
            _ERROR <> ": Missing key: \'" <> key <> "\'.                                     \n\
            \                                                                                \n\
            \Explanation: Key present in Dhall type (and not optional) is not provided       \n\
            \in CSV. Please make sure every field key (non-optional) in Dhall type is        \n\
            \present in CSV header.                                                          \n\
            \                                                                                \n\
            \If working with headerless CSVs, fields in Dhall type should have keys          \n\
            \_1, _2, _3, ... and so forth                                                    "

    show (UnhandledKeys keys) =
        Data.Text.unpack $
            _ERROR <> ": Following key(s): " <> (Data.Text.intercalate ", " keys) <>        "\n\
            \are not handled.                                                                \n\
            \                                                                                \n\
            \Explanation: Keys present in CSV header are not present in Dhall type.          \n\
            \You may turn off the --strict-recs flag to ignore this error.                   "

    show (Mismatch tp field key) =
        Data.Text.unpack $
            _ERROR <> ": Type Mismtatch with key: " <> key <>                               "\n\
            \                                                                                \n\
            \Explanation: Could not parse CSV field " <> field <>                           "\n\
            \into the expected Dhall type:                                                   \n\
            \                                                                                \n\
            \" <> insert tp

    show (ContainsUnion e) =
        Data.Text.unpack $
            _ERROR <> ": Dhall type contains a Union type.                                   \n\
            \                                                                                \n\
            \Explanation: Dhall type contains a Union type for one of the record fields.     \n\
            \This error occurs because flag --unions-none is turned on. If it is desired to  \n\
            \have unions in the parsed expression, disable --unions-none flag.               \n\
            \                                                                                \n\
            \Expected no unions but got the following type in schema:                        \n\
            \                                                                                \n\
            \" <> insert e

    show (UndecidableUnion tp field key opts) =
        Data.Text.unpack $
            _ERROR <> ": A union typed field can be parsed in more than one way.             \n\
            \                                                                                \n\
            \Explanation: The CSV field " <> field <>                                       "\n\
            \             with key " <> key <>                                              "\n\
            \can be converted in more than one of the expected union type alternatives.      \n\
            \This error occurs because flag --unions-strict is turned on. You may turn off   \n\
            \this flag to select the first valid alternative found.                          \n\
            \                                                                                \n\
            \Expected union type:                                                            \n\
            \                                                                                \n\
            \" <> insert tp <>
            "\nField can be parsed as the following expressions: \n"
            <> Data.Text.intercalate
            "\n------------------------------------------------------------------------------\n"
            (map insert opts)

    show (UndecidableMissingUnion tp key opts) =
        Data.Text.unpack $
            _ERROR <> ": A union typed field can be parsed in more than one way.             \n\
            \                                                                                \n\
            \Explanation: The record field key " <> key <>                                  "\n\
            \             missing in CSV                                                     \n\
            \can be converted in more than one of the expected union type alternatives.      \n\
            \That is to say, there are more than one alternatives with Optional types.       \n\
            \This error occurs because flag --unions-strict is turned on. You may turn off   \n\
            \this flag to select the first valid alternative found.                          \n\
            \                                                                                \n\
            \Expected union type:                                                            \n\
            \                                                                                \n\
            \" <> insert tp <>
            "\nMissing field can be parsed as the following expressions:                     \n\
            \" <> Data.Text.intercalate
            "\n------------------------------------------------------------------------------\n"
            (map insert opts)

    show (UnicodeError e) = show e

instance Exception CompileError

insert :: Pretty a => a -> Text
insert = Pretty.renderStrict . Dhall.Pretty.layout . Dhall.Util.insert
