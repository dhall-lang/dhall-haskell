{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-| This library only exports a single `dhallToJSON` function for translating a
    Dhall syntax tree to a JSON syntax tree (i.e. a `Value`) for the @aeson@
    library

    NOTE: The @yaml@ library uses the same `Value` type to represent YAML
    files, so you can use this to convert Dhall expressions to YAML, too

    See the @dhall@ package if you would like to transform Dhall source code
    into a Dhall syntax tree.  Similarly, see the @aeson@ package if you would
    like to translate a JSON syntax tree into JSON.

    This package also provides @dhall-to-json@ and @dhall-to-yaml@ executables
    which you can use to compile Dhall source code directly to JSON or YAML for
    your convenience

    Not all Dhall expressions can be converted to JSON since JSON is not a
    programming language.  The only things you can convert are:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@
    * @List@s
    * @Optional@ values
    * unions
    * records

    Dhall @Bool@s translate to JSON bools:

> $ dhall-to-json <<< 'True'
> true
> $ dhall-to-json <<< 'False'
> false

    Dhall numbers translate to JSON numbers:

> $ dhall-to-json <<< '+2'
> 2
> $ dhall-to-json <<< '2'
> 2
> $ dhall-to-json <<< '2.3'
> 2.3

    Dhall @Text@ translates to JSON text:

> $ dhall-to-json <<< '"ABC"'
> "ABC"

    Dhall @List@s translate to JSON lists:

> $ dhall-to-json <<< '[1, 2, 3] : List Integer'
> [1,2,3]

    Dhall @Optional@ values translate to @null@ if absent and the unwrapped
    value otherwise:

> $ dhall-to-json <<< '[] : Optional Integer'
> null
> $ dhall-to-json <<< '[1] : Optional Integer'
> 1

    Dhall records translate to JSON records:

> $ dhall-to-json <<< '{ foo = 1, bar = True }'
> {"foo":1,"bar":true}

    Dhall unions translate to the wrapped value:

> $ dhall-to-json <<< "< Left = +2 | Right : Natural>"
> 2
> $ cat config
> [ < Person = { age = +47, name = "John" }
>   | Place  : { location : Text }
>   >
> , < Place  = { location = "North Pole" }
>   | Person : { age : Natural, name : Text }
>   >
> , < Place  = { location = "Sahara Desert" }
>   | Person : { age : Natural, name : Text }
>   >
> , < Person = { age = +35, name = "Alice" }
>   | Place  : { location : Text }
>   >
> ]
> $ dhall-to-json <<< "./config"
> [{"age":47,"name":"John"},{"location":"North Pole"},{"location":"Sahara Desert"},{"age":35,"name":"Alice"}]

    You can preserve the name of the alternative if you wrap the value in a
    record with three fields:

    * @contents@: The union literal that you want to preserve the tag of

    * @field@: the name of the field that will store the name of the
      alternative

    * @nesting@: A value of type @\< Inline : {} | Nested : Text \>@.

    If @nesting@ is set to @Inline@ and the union literal stored in @contents@
    contains a record then the name of the alternative is stored inline within
    the same record.  For example, this code:

>     let Example = < Left : { foo : Natural } | Right : { bar : Bool } >
> 
> in  let example = constructors Example
> 
> in  let Nesting = < Inline : {} | Nested : Text >
> 
> in  let nesting = constructors Nesting
> 
> in  { field    = "name"
>     , nesting  = nesting.Inline {=}
>     , contents = example.Left { foo = 2 }
>     }

    ... produces this JSON:

> {
>   "foo": 2,
>   "name": "Left"
> }

    If @nesting@ is set to @Nested nestedField@ then the union is store
    underneath a field named @nestedField@.  For example, this code:

>     let Example = < Left : { foo : Natural } | Right : { bar : Bool } >
> 
> in  let example = constructors Example
> 
> in  let Nesting = < Inline : {} | Nested : Text >
> 
> in  let nesting = constructors Nesting
> 
> in  { field    = "name"
>     , nesting  = nesting.Nested "value"
>     , contents = example.Left { foo = 2 }
>     }

    ... produces this JSON:

> {
>   "name": "Left",
>   "value": {
>     "foo": 2
>   }
> }

    Also, all Dhall expressions are normalized before translation to JSON:

> $ dhall-to-json <<< "True == False"
> false

-}

module Dhall.JSON (
    -- * Dhall to JSON
      dhallToJSON
    , omitNull
    , omitEmpty
    , parseOmission
    , Conversion(..)
    , convertToHomogeneousMaps
    , parseConversion
    , codeToValue

    -- * Exceptions
    , CompileError(..)
    ) where

import Control.Applicative (empty, (<|>))
import Control.Monad (guard)
import Control.Exception (Exception, throwIO)
import Data.Aeson (Value(..), ToJSON(..))
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)
import Dhall.Map (Map)
import Options.Applicative (Parser)

import qualified Data.Foldable
import qualified Data.HashMap.Strict
import qualified Data.List
import qualified Data.Ord
import qualified Data.Text
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Options.Applicative

{-| This is the exception type for errors that might arise when translating
    Dhall to JSON

    Because the majority of Dhall language features do not translate to JSON
    this just returns the expression that failed
-}
data CompileError = Unsupported (Expr X X) deriving (Typeable)

instance Show CompileError where
    show (Unsupported e) =
        Data.Text.unpack $
            "" <> _ERROR <> ": Cannot translate to JSON                                     \n\
            \                                                                               \n\
            \Explanation: Only primitive values, records, unions, ❰List❱s, and ❰Optional❱   \n\
            \values can be translated from Dhall to JSON                                    \n\
            \                                                                               \n\
            \The following Dhall expression could not be translated to JSON:                \n\
            \                                                                               \n\
            \↳ " <> txt <> "                                                                "
      where
        txt = Dhall.Core.pretty e

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Exception CompileError

{-| Convert a Dhall expression to the equivalent JSON expression

>>> :set -XOverloadedStrings
>>> :set -XOverloadedLists
>>> import Dhall.Core
>>> dhallToJSON (RecordLit [("foo", IntegerLit 1), ("bar", TextLit "ABC")])
Right (Object (fromList [("foo",Number 1.0),("bar",String "ABC")]))
>>> fmap Data.Aeson.encode it
Right "{\"foo\":1,\"bar\":\"ABC\"}"
-}
dhallToJSON :: Expr s X -> Either CompileError Value
dhallToJSON e0 = loop (Dhall.Core.normalize e0)
  where
    loop e = case e of 
        Dhall.Core.BoolLit a -> return (toJSON a)
        Dhall.Core.NaturalLit a -> return (toJSON a)
        Dhall.Core.IntegerLit a -> return (toJSON a)
        Dhall.Core.DoubleLit a
          | isInfinite a && a > 0 -> return (toJSON ( 1.7976931348623157e308 :: Double))
          | isInfinite a && a < 0 -> return (toJSON (-1.7976931348623157e308 :: Double))
          | otherwise -> return (toJSON a)
        Dhall.Core.TextLit (Dhall.Core.Chunks [] a) -> do
            return (toJSON a)
        Dhall.Core.ListLit _ a -> do
            a' <- traverse loop a
            return (toJSON a')
        Dhall.Core.OptionalLit _ a -> do
            a' <- traverse loop a
            return (toJSON a')
        Dhall.Core.Some a -> do
            a' <- loop a
            return (toJSON a')
        Dhall.Core.App Dhall.Core.None _ -> do
            return Data.Aeson.Null
        Dhall.Core.RecordLit a ->
            case toOrderedList a of
                [   (   "contents"
                    ,   Dhall.Core.UnionLit alternativeName contents _
                    )
                 ,  (   "field"
                    ,   Dhall.Core.TextLit
                            (Dhall.Core.Chunks [] field)
                    )
                 ,  (   "nesting"
                    ,   Dhall.Core.UnionLit
                            "Nested"
                            (Dhall.Core.TextLit
                                (Dhall.Core.Chunks [] nestedField)
                            )
                            [ ("Inline", Just (Dhall.Core.Record [])) ]
                    )
                 ] -> do
                    contents' <- loop contents

                    let taggedValue =
                            Dhall.Map.fromList
                                [   (   field
                                    ,   toJSON alternativeName
                                    )
                                ,   (   nestedField
                                    ,   contents'
                                    )
                                ]

                    return (Data.Aeson.toJSON ( Dhall.Map.toMap taggedValue ))

                [   (   "contents"
                    ,   Dhall.Core.UnionLit
                            alternativeName
                            (Dhall.Core.RecordLit contents)
                            _
                    )
                 ,  (   "field"
                    ,   Dhall.Core.TextLit
                            (Dhall.Core.Chunks [] field)
                    )
                 ,  (   "nesting"
                    ,   Dhall.Core.UnionLit
                            "Inline"
                            (Dhall.Core.RecordLit [])
                            [ ("Nested", Just Dhall.Core.Text) ]
                    )
                 ] -> do
                    let contents' =
                            Dhall.Map.insert
                                field
                                (Dhall.Core.TextLit
                                    (Dhall.Core.Chunks
                                        []
                                        alternativeName
                                    )
                                )
                                contents

                    loop (Dhall.Core.RecordLit contents')
                _ -> do
                    a' <- traverse loop a
                    return (Data.Aeson.toJSON (Dhall.Map.toMap a'))
        Dhall.Core.UnionLit _ b _ -> loop b
        Dhall.Core.App (Dhall.Core.Field (Dhall.Core.Union _) _) b -> loop b
        Dhall.Core.Field (Dhall.Core.Union _) k -> return (toJSON k)
        _ -> Left (Unsupported e)

toOrderedList :: Ord k => Map k v -> [(k, v)]
toOrderedList =
        Data.List.sortBy (Data.Ord.comparing fst)
    .   Dhall.Map.toList

-- | Omit record fields that are @null@
omitNull :: Value -> Value
omitNull (Object object) = Object fields
  where
    fields =Data.HashMap.Strict.filter (/= Null) (fmap omitNull object)
omitNull (Array array) =
    Array (fmap omitNull array)
omitNull (String string) =
    String string
omitNull (Number number) =
    Number number
omitNull (Bool bool) =
    Bool bool
omitNull Null =
    Null

{-| Omit record fields that are @null@, arrays and records whose transitive 
    fields are all null
-}
omitEmpty :: Value -> Value
omitEmpty (Object object) =
    if null fields then Null else Object fields
  where
    fields = Data.HashMap.Strict.filter (/= Null) (fmap omitEmpty object)
omitEmpty (Array array) =
    if null elems then Null else Array elems
  where
    elems = (fmap omitEmpty array)
omitEmpty (String string) =
    String string
omitEmpty (Number number) =
    Number number
omitEmpty (Bool bool) =
    Bool bool
omitEmpty Null =
    Null

-- | Parser for command-line options related to omitting fields
parseOmission :: Parser (Value -> Value)
parseOmission =
        Options.Applicative.flag'
            omitNull
            (   Options.Applicative.long "omitNull"
            <>  Options.Applicative.help "Omit record fields that are null"
            )
    <|> Options.Applicative.flag'
            omitEmpty
            (   Options.Applicative.long "omitEmpty"
            <>  Options.Applicative.help "Omit record fields that are null or empty records"
            )
    <|> pure id

{-| Specify whether or not to convert association lists of type
    @List { mapKey: Text, mapValue : v }@ to records
-}
data Conversion
    = NoConversion
    | Conversion { mapKey :: Text, mapValue :: Text }

{-| Convert association lists to homogeneous maps

    This converts an association list of the form:

    > [ { mapKey = k0, mapValue = v0 }, { mapKey = k1, mapValue = v1 } ]

    ... to a record of the form:

    > { k0 = v0, k1 = v1 }
-}
convertToHomogeneousMaps :: Conversion -> Expr s X -> Expr s X
convertToHomogeneousMaps NoConversion e0 = e0
convertToHomogeneousMaps (Conversion {..}) e0 = loop (Dhall.Core.normalize e0)
  where
    loop e = case e of
        Dhall.Core.Const a ->
            Dhall.Core.Const a

        Dhall.Core.Var v ->
            Dhall.Core.Var v

        Dhall.Core.Lam a b c ->
            Dhall.Core.Lam a b' c'
          where
            b' = loop b
            c' = loop c

        Dhall.Core.Pi a b c ->
            Dhall.Core.Pi a b' c'
          where
            b' = loop b
            c' = loop c

        Dhall.Core.App a b ->
            Dhall.Core.App a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Let as b ->
            Dhall.Core.Let as' b'
          where
            f (Dhall.Core.Binding x y z) = Dhall.Core.Binding x y' z'
              where
                y' = fmap loop y
                z' =      loop z

            as' = fmap f as

            b' = loop b

        Dhall.Core.Annot a b ->
            Dhall.Core.Annot a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Bool ->
            Dhall.Core.Bool

        Dhall.Core.BoolLit a ->
            Dhall.Core.BoolLit a

        Dhall.Core.BoolAnd a b ->
            Dhall.Core.BoolAnd a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolOr a b ->
            Dhall.Core.BoolOr a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolEQ a b ->
            Dhall.Core.BoolEQ a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolNE a b ->
            Dhall.Core.BoolNE a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolIf a b c ->
            Dhall.Core.BoolIf a' b' c'
          where
            a' = loop a
            b' = loop b
            c' = loop c

        Dhall.Core.Natural ->
            Dhall.Core.Natural

        Dhall.Core.NaturalLit a ->
            Dhall.Core.NaturalLit a

        Dhall.Core.NaturalFold ->
            Dhall.Core.NaturalFold

        Dhall.Core.NaturalBuild ->
            Dhall.Core.NaturalBuild

        Dhall.Core.NaturalIsZero ->
            Dhall.Core.NaturalIsZero

        Dhall.Core.NaturalEven ->
            Dhall.Core.NaturalEven

        Dhall.Core.NaturalOdd ->
            Dhall.Core.NaturalOdd

        Dhall.Core.NaturalToInteger ->
            Dhall.Core.NaturalToInteger

        Dhall.Core.NaturalShow ->
            Dhall.Core.NaturalShow

        Dhall.Core.NaturalPlus a b ->
            Dhall.Core.NaturalPlus a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.NaturalTimes a b ->
            Dhall.Core.NaturalTimes a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Integer ->
            Dhall.Core.Integer

        Dhall.Core.IntegerLit a ->
            Dhall.Core.IntegerLit a

        Dhall.Core.IntegerShow ->
            Dhall.Core.IntegerShow

        Dhall.Core.IntegerToDouble ->
            Dhall.Core.IntegerToDouble

        Dhall.Core.Double ->
            Dhall.Core.Double

        Dhall.Core.DoubleLit a ->
            Dhall.Core.DoubleLit a

        Dhall.Core.DoubleShow ->
            Dhall.Core.DoubleShow

        Dhall.Core.Text ->
            Dhall.Core.Text

        Dhall.Core.TextLit (Dhall.Core.Chunks a b) ->
            Dhall.Core.TextLit (Dhall.Core.Chunks a' b)
          where
            a' = fmap (fmap loop) a

        Dhall.Core.TextAppend a b ->
            Dhall.Core.TextAppend a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.TextShow ->
            Dhall.Core.TextShow

        Dhall.Core.List ->
            Dhall.Core.List

        Dhall.Core.ListLit a b ->
            case transform of
                Just c  -> loop c
                Nothing -> Dhall.Core.ListLit a' b'
          where
            elements = Data.Foldable.toList b

            toKeyValue :: Expr s X -> Maybe (Text, Expr s X)
            toKeyValue (Dhall.Core.RecordLit m) = do
                guard (Data.Foldable.length m == 2)

                key   <- Dhall.Map.lookup mapKey   m
                value <- Dhall.Map.lookup mapValue m

                keyText <- case key of
                    Dhall.Core.TextLit (Dhall.Core.Chunks [] keyText) ->
                        return keyText

                    _ ->
                        empty

                return (keyText, value)
            toKeyValue _ = do
                empty

            transform =
                case elements of
                    [] ->
                        case a of
                            Just (Dhall.Core.Record m) -> do
                                guard (Data.Foldable.length m == 2)
                                guard (Dhall.Map.member mapKey   m)
                                guard (Dhall.Map.member mapValue m)
                                return (Dhall.Core.RecordLit mempty)
                            _ -> do
                                empty

                    _  -> do
                        keyValues <- traverse toKeyValue elements

                        let recordLiteral =
                                Dhall.Map.fromList keyValues

                        return (Dhall.Core.RecordLit recordLiteral)

            a' = fmap loop a
            b' = fmap loop b

        Dhall.Core.ListAppend a b ->
            Dhall.Core.ListAppend a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.ListBuild ->
            Dhall.Core.ListBuild

        Dhall.Core.ListFold ->
            Dhall.Core.ListFold

        Dhall.Core.ListLength ->
            Dhall.Core.ListLength

        Dhall.Core.ListHead ->
            Dhall.Core.ListHead

        Dhall.Core.ListLast ->
            Dhall.Core.ListLast

        Dhall.Core.ListIndexed ->
            Dhall.Core.ListIndexed

        Dhall.Core.ListReverse ->
            Dhall.Core.ListReverse

        Dhall.Core.Optional ->
            Dhall.Core.Optional

        Dhall.Core.OptionalLit a b ->
            Dhall.Core.OptionalLit a' b'
          where
            a' =      loop a
            b' = fmap loop b

        Dhall.Core.Some a ->
            Dhall.Core.Some a'
          where
            a' = loop a

        Dhall.Core.None ->
            Dhall.Core.None

        Dhall.Core.OptionalFold ->
            Dhall.Core.OptionalFold

        Dhall.Core.OptionalBuild ->
            Dhall.Core.OptionalBuild

        Dhall.Core.Record a ->
            Dhall.Core.Record a'
          where
            a' = fmap loop a

        Dhall.Core.RecordLit a ->
            Dhall.Core.RecordLit a'
          where
            a' = fmap loop a

        Dhall.Core.Union a ->
            Dhall.Core.Union a'
          where
            a' = fmap (fmap loop) a

        Dhall.Core.UnionLit a b c ->
            Dhall.Core.UnionLit a b' c'
          where
            b' =            loop  b
            c' = fmap (fmap loop) c

        Dhall.Core.Combine a b ->
            Dhall.Core.Combine a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.CombineTypes a b ->
            Dhall.Core.CombineTypes a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Prefer a b ->
            Dhall.Core.Prefer a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Merge a b c ->
            Dhall.Core.Merge a' b' c'
          where
            a' =      loop a
            b' =      loop b
            c' = fmap loop c

        Dhall.Core.Field a b ->
            Dhall.Core.Field a' b
          where
            a' = loop a

        Dhall.Core.Project a b ->
            Dhall.Core.Project a' b
          where
            a' = loop a

        Dhall.Core.ImportAlt a b ->
            Dhall.Core.ImportAlt a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Note a b ->
            Dhall.Core.Note a b'
          where
            b' = loop b

        Dhall.Core.Embed a ->
            Dhall.Core.Embed a

-- | Parser for command-line options related to homogeneous map support
parseConversion :: Parser Conversion
parseConversion =
        conversion
    <|> noConversion
  where
    conversion = Conversion <$> parseKeyField <*> parseValueField
      where
        parseKeyField =
            Options.Applicative.strOption
                (   Options.Applicative.long "key"
                <>  Options.Applicative.help "Reserved key field name for association lists"
                <>  Options.Applicative.value "mapKey"
                <>  Options.Applicative.showDefaultWith Data.Text.unpack
                )

        parseValueField =
            Options.Applicative.strOption
                (   Options.Applicative.long "value"
                <>  Options.Applicative.help "Reserved value field name for association lists"
                <>  Options.Applicative.value "mapValue"
                <>  Options.Applicative.showDefaultWith Data.Text.unpack
                )

    noConversion =
        Options.Applicative.flag'
            NoConversion
            (   Options.Applicative.long "noMaps"
            <>  Options.Applicative.help "Disable conversion of association lists to homogeneous maps"
            )


{-| Convert a piece of Text carrying a Dhall inscription to an equivalent JSON Value

>>> :set -XOverloadedStrings
>>> import Dhall.Core
>>> Dhall.JSON.codeToValue "(stdin)" "{ a = 1 }"
>>> Object (fromList [("a",Number 1.0)])
-}
codeToValue
  :: Conversion
  -> Text  -- ^ Describe the input for the sake of error location.
  -> Text  -- ^ Input text.
  -> IO Value
codeToValue conversion name code = do
    parsedExpression <- case Dhall.Parser.exprFromText (Data.Text.unpack name) code of
      Left  err              -> Control.Exception.throwIO err
      Right parsedExpression -> return parsedExpression

    resolvedExpression <- Dhall.Import.load parsedExpression

    case Dhall.TypeCheck.typeOf resolvedExpression  of
      Left  err -> Control.Exception.throwIO err
      Right _   -> return ()

    let convertedExpression =
            convertToHomogeneousMaps conversion resolvedExpression

    case dhallToJSON convertedExpression of
      Left  err  -> Control.Exception.throwIO err
      Right json -> return json
