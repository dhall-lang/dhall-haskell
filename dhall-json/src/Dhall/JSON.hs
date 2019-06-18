{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

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

    You can also translate Dhall expressions encoding weakly-typed JSON
    (see: <https://prelude.dhall-lang.org/JSON/Type>):

> $ cat ./example.dhall
> let JSON = https://prelude.dhall-lang.org/JSON/package.dhall
> 
> in  JSON.object
>     [ { mapKey = "foo", mapValue = JSON.null }
>     , { mapKey =
>           "bar"
>       , mapValue =
>           JSON.array [ JSON.number 1.0, JSON.bool True ]
>       }
>     ]

> $ dhall-to-json <<< './example.dhall'
> {"foo":null,"bar":[1,true]}

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
    , SpecialDoubleMode(..)
    , handleSpecialDoubles
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
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)
import Dhall.Map (Map)
import Dhall.JSON.Util (pattern V)
import Options.Applicative (Parser)

import qualified Control.Lens
import qualified Data.Aeson          as Aeson
import qualified Data.Foldable       as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Data.Ord
import qualified Data.Text
import qualified Data.Vector         as Vector
import qualified Dhall.Core          as Core
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
data CompileError
    = Unsupported (Expr X X)
    | SpecialDouble Double
    | BareNone

instance Show CompileError where
    show BareNone =
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
            \The conversion to JSON/YAML only translates the fully applied form to ❰null❱.   "

    show (SpecialDouble n) =
       Data.Text.unpack $
            _ERROR <> ": " <> special <> " disallowed in JSON                                         \n\
            \                                                                                \n\
            \Explanation: The JSON standard does not define a canonical way to encode        \n\
            \❰NaN❱/❰Infinity❱/❰-Infinity❱.  You can fix this error by either:                \n\
            \                                                                                \n\
            \● Using ❰dhall-to-yaml❱ instead of ❰dhall-to-json❱, since YAML does support     \n\
            \  ❰NaN❱/❰Infinity❱/❰-Infinity❱                                                  \n\
            \                                                                                \n\
            \● Enabling the ❰--approximate-special-doubles❱ flag which will encode ❰NaN❱ as  \n\
            \  ❰null❱, ❰Infinity❱ as the maximum ❰Double❱, and ❰-Infinity❱ as the minimum    \n\
            \❰Double❱                                                                        \n\
            \                                                                                \n\
            \● See if there is a way to remove ❰NaN❱/❰Infinity❱/❰-Infinity❱ from the         \n\
            \  expression that you are converting to JSON                                    "
      where
        special = Data.Text.pack (show n)

    show (Unsupported e) =
        Data.Text.unpack $
            _ERROR <> ": Cannot translate to JSON                                            \n\
            \                                                                                \n\
            \Explanation: Only primitive values, records, unions, ❰List❱s, and ❰Optional❱    \n\
            \values can be translated from Dhall to JSON                                     \n\
            \                                                                                \n\
            \The following Dhall expression could not be translated to JSON:                 \n\
            \                                                                                \n\
            \↳ " <> txt <> "                                                                 "
      where
        txt = Core.pretty e

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Exception CompileError

{-| Convert a Dhall expression to the equivalent JSON expression

>>> :set -XOverloadedStrings
>>> :set -XOverloadedLists
>>> import Core
>>> dhallToJSON (RecordLit [("foo", IntegerLit 1), ("bar", TextLit "ABC")])
Right (Object (fromList [("foo",Number 1.0),("bar",String "ABC")]))
>>> fmap Aeson.encode it
Right "{\"foo\":1,\"bar\":\"ABC\"}"
-}
dhallToJSON
    :: Expr s X
    -> Either CompileError Value
dhallToJSON e0 = loop (Core.alphaNormalize (Core.normalize e0))
  where
    loop e = case e of 
        Core.BoolLit a -> return (toJSON a)
        Core.NaturalLit a -> return (toJSON a)
        Core.IntegerLit a -> return (toJSON a)
        Core.DoubleLit a -> return (toJSON a)
        Core.TextLit (Core.Chunks [] a) -> do
            return (toJSON a)
        Core.ListLit _ a -> do
            a' <- traverse loop a
            return (toJSON a')
        Core.Some a -> do
            a' <- loop a
            return (toJSON a')
        Core.App Core.None _ -> do
            return Aeson.Null
        -- Provide a nicer error message for a common user mistake.
        --
        -- See: https://github.com/dhall-lang/dhall-lang/issues/492
        Core.None -> do
            Left BareNone
        Core.RecordLit a ->
            case toOrderedList a of
                [   (   "contents"
                    ,   Core.UnionLit alternativeName contents _
                    )
                 ,  (   "field"
                    ,   Core.TextLit
                            (Core.Chunks [] field)
                    )
                 ,  (   "nesting"
                    ,   Core.UnionLit
                            "Nested"
                            (Core.TextLit
                                (Core.Chunks [] nestedField)
                            )
                            [ ("Inline", Just (Core.Record [])) ]
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

                    return (Aeson.toJSON ( Dhall.Map.toMap taggedValue ))

                [   (   "contents"
                    ,   Core.UnionLit
                            alternativeName
                            (Core.RecordLit contents)
                            _
                    )
                 ,  (   "field"
                    ,   Core.TextLit
                            (Core.Chunks [] field)
                    )
                 ,  (   "nesting"
                    ,   Core.UnionLit
                            "Inline"
                            (Core.RecordLit [])
                            [ ("Nested", Just Core.Text) ]
                    )
                 ] -> do
                    let contents' =
                            Dhall.Map.insert
                                field
                                (Core.TextLit
                                    (Core.Chunks
                                        []
                                        alternativeName
                                    )
                                )
                                contents

                    loop (Core.RecordLit contents')
                _ -> do
                    a' <- traverse loop a
                    return (Aeson.toJSON (Dhall.Map.toMap a'))
        Core.UnionLit _ b _ -> loop b
        Core.App (Core.Field (Core.Union _) _) b -> loop b
        Core.Field (Core.Union _) k -> return (Aeson.toJSON k)
        Core.Lam _ (Core.Const Core.Type)
            (Core.Lam _
                (Core.Record
                    [ ("array" , Core.Pi _ (Core.App Core.List (V 0)) (V 1))
                    , ("bool"  , Core.Pi _ Core.Bool (V 1))
                    , ("null"  , V 0)
                    , ("number", Core.Pi _ Core.Double (V 1))
                    , ("object", Core.Pi _ (Core.App Core.List (Core.Record [ ("mapKey", Core.Text), ("mapValue", V 0)])) (V 1))
                    , ("string", Core.Pi _ Core.Text (V 1))
                    ]
                )
                value
            ) -> do
                let outer (Core.Field (V 0) "null") = do
                        return Aeson.Null
                    outer (Core.App (Core.Field (V 0) "bool") (Core.BoolLit b)) = do
                        return (Aeson.Bool b)
                    outer (Core.App (Core.Field (V 0) "array") (Core.ListLit _ xs)) = do
                        ys <- traverse outer (Foldable.toList xs)

                        return (Aeson.Array (Vector.fromList ys))
                    outer (Core.App (Core.Field (V 0) "object") (Core.ListLit _ xs)) = do
                        let inner (Core.RecordLit [("mapKey", Core.TextLit (Core.Chunks [] mapKey)), ("mapValue", mapExpression)]) = do
                                mapValue <- outer mapExpression

                                return (mapKey, mapValue)
                            inner _ = Left (Unsupported e)

                        ys <- traverse inner (Foldable.toList xs)

                        return (Aeson.Object (HashMap.fromList ys))
                    outer (Core.App (Core.Field (V 0) "number") (Core.DoubleLit n)) = do
                        return (Aeson.toJSON n)
                    outer (Core.App (Core.Field (V 0) "string") (Core.TextLit (Core.Chunks [] text))) = do
                        return (toJSON text)
                    outer _ = Left (Unsupported e)

                outer value
        _ -> Left (Unsupported e)

toOrderedList :: Ord k => Map k v -> [(k, v)]
toOrderedList =
        Data.List.sortBy (Data.Ord.comparing fst)
    .   Dhall.Map.toList

-- | Omit record fields that are @null@
omitNull :: Value -> Value
omitNull (Object object) = Object fields
  where
    fields =HashMap.filter (/= Null) (fmap omitNull object)
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
    fields = HashMap.filter (/= Null) (fmap omitEmpty object)
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
convertToHomogeneousMaps (Conversion {..}) e0 = loop (Core.normalize e0)
  where
    loop e = case e of
        Core.Const a ->
            Core.Const a

        Core.Var v ->
            Core.Var v

        {- Minor hack: Don't descend into lambda, since the only thing it can
           possibly encode is a Boehm-Berarducci-encoded JSON value.  In such a
           case we do *not* want to perform this rewrite since it will
           interfere with decoding the value.
        -}
        Core.Lam a b c ->
            Core.Lam a b c

        Core.Pi a b c ->
            Core.Pi a b' c'
          where
            b' = loop b
            c' = loop c

        Core.App a b ->
            Core.App a' b'
          where
            a' = loop a
            b' = loop b

        Core.Let as b ->
            Core.Let as' b'
          where
            f (Core.Binding x y z) = Core.Binding x y' z'
              where
                y' = fmap loop y
                z' =      loop z

            as' = fmap f as

            b' = loop b

        Core.Annot a b ->
            Core.Annot a' b'
          where
            a' = loop a
            b' = loop b

        Core.Bool ->
            Core.Bool

        Core.BoolLit a ->
            Core.BoolLit a

        Core.BoolAnd a b ->
            Core.BoolAnd a' b'
          where
            a' = loop a
            b' = loop b

        Core.BoolOr a b ->
            Core.BoolOr a' b'
          where
            a' = loop a
            b' = loop b

        Core.BoolEQ a b ->
            Core.BoolEQ a' b'
          where
            a' = loop a
            b' = loop b

        Core.BoolNE a b ->
            Core.BoolNE a' b'
          where
            a' = loop a
            b' = loop b

        Core.BoolIf a b c ->
            Core.BoolIf a' b' c'
          where
            a' = loop a
            b' = loop b
            c' = loop c

        Core.Natural ->
            Core.Natural

        Core.NaturalLit a ->
            Core.NaturalLit a

        Core.NaturalFold ->
            Core.NaturalFold

        Core.NaturalBuild ->
            Core.NaturalBuild

        Core.NaturalIsZero ->
            Core.NaturalIsZero

        Core.NaturalEven ->
            Core.NaturalEven

        Core.NaturalOdd ->
            Core.NaturalOdd

        Core.NaturalToInteger ->
            Core.NaturalToInteger

        Core.NaturalShow ->
            Core.NaturalShow

        Core.NaturalPlus a b ->
            Core.NaturalPlus a' b'
          where
            a' = loop a
            b' = loop b

        Core.NaturalTimes a b ->
            Core.NaturalTimes a' b'
          where
            a' = loop a
            b' = loop b

        Core.Integer ->
            Core.Integer

        Core.IntegerLit a ->
            Core.IntegerLit a

        Core.IntegerShow ->
            Core.IntegerShow

        Core.IntegerToDouble ->
            Core.IntegerToDouble

        Core.Double ->
            Core.Double

        Core.DoubleLit a ->
            Core.DoubleLit a

        Core.DoubleShow ->
            Core.DoubleShow

        Core.Text ->
            Core.Text

        Core.TextLit (Core.Chunks a b) ->
            Core.TextLit (Core.Chunks a' b)
          where
            a' = fmap (fmap loop) a

        Core.TextAppend a b ->
            Core.TextAppend a' b'
          where
            a' = loop a
            b' = loop b

        Core.TextShow ->
            Core.TextShow

        Core.List ->
            Core.List

        Core.ListLit a b ->
            case transform of
                Just c  -> loop c
                Nothing -> Core.ListLit a' b'
          where
            elements = Foldable.toList b

            toKeyValue :: Expr s X -> Maybe (Text, Expr s X)
            toKeyValue (Core.RecordLit m) = do
                guard (Foldable.length m == 2)

                key   <- Dhall.Map.lookup mapKey   m
                value <- Dhall.Map.lookup mapValue m

                keyText <- case key of
                    Core.TextLit (Core.Chunks [] keyText) ->
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
                            Just (Core.Record m) -> do
                                guard (Foldable.length m == 2)
                                guard (Dhall.Map.member mapKey   m)
                                guard (Dhall.Map.member mapValue m)
                                return (Core.RecordLit mempty)
                            _ -> do
                                empty

                    _  -> do
                        keyValues <- traverse toKeyValue elements

                        let recordLiteral =
                                Dhall.Map.fromList keyValues

                        return (Core.RecordLit recordLiteral)

            a' = fmap loop a
            b' = fmap loop b

        Core.ListAppend a b ->
            Core.ListAppend a' b'
          where
            a' = loop a
            b' = loop b

        Core.ListBuild ->
            Core.ListBuild

        Core.ListFold ->
            Core.ListFold

        Core.ListLength ->
            Core.ListLength

        Core.ListHead ->
            Core.ListHead

        Core.ListLast ->
            Core.ListLast

        Core.ListIndexed ->
            Core.ListIndexed

        Core.ListReverse ->
            Core.ListReverse

        Core.Optional ->
            Core.Optional

        Core.Some a ->
            Core.Some a'
          where
            a' = loop a

        Core.None ->
            Core.None

        Core.OptionalFold ->
            Core.OptionalFold

        Core.OptionalBuild ->
            Core.OptionalBuild

        Core.Record a ->
            Core.Record a'
          where
            a' = fmap loop a

        Core.RecordLit a ->
            Core.RecordLit a'
          where
            a' = fmap loop a

        Core.Union a ->
            Core.Union a'
          where
            a' = fmap (fmap loop) a

        Core.UnionLit a b c ->
            Core.UnionLit a b' c'
          where
            b' =            loop  b
            c' = fmap (fmap loop) c

        Core.Combine a b ->
            Core.Combine a' b'
          where
            a' = loop a
            b' = loop b

        Core.CombineTypes a b ->
            Core.CombineTypes a' b'
          where
            a' = loop a
            b' = loop b

        Core.Prefer a b ->
            Core.Prefer a' b'
          where
            a' = loop a
            b' = loop b

        Core.Merge a b c ->
            Core.Merge a' b' c'
          where
            a' =      loop a
            b' =      loop b
            c' = fmap loop c

        Core.Field a b ->
            Core.Field a' b
          where
            a' = loop a

        Core.Project a b ->
            Core.Project a' b
          where
            a' = loop a

        Core.ImportAlt a b ->
            Core.ImportAlt a' b'
          where
            a' = loop a
            b' = loop b

        Core.Note a b ->
            Core.Note a b'
          where
            b' = loop b

        Core.Embed a ->
            Core.Embed a

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

-- | This option specifies how to encode @NaN@\/@Infinity@\/@-Infinity@
data SpecialDoubleMode
    = UseYAMLEncoding
    -- ^ YAML natively supports @NaN@\/@Infinity@\/@-Infinity@
    | ForbidWithinJSON
    -- ^ Forbid @NaN@\/@Infinity@\/@-Infinity@ because JSON doesn't support them
    | ApproximateWithinJSON
    -- ^ Encode @NaN@\/@Infinity@\/@-Infinity@ as
    --   @null@\/@1.7976931348623157e308@\/@-1.7976931348623157e308@,
    --   respectively

{-| Pre-process an expression containing @NaN@\/@Infinity@\/@-Infinity@,
    handling them as specified according to the `SpecialDoubleMode`
-}
handleSpecialDoubles
    :: SpecialDoubleMode -> Expr s X -> Either CompileError (Expr s X)
handleSpecialDoubles specialDoubleMode =
    Control.Lens.rewriteMOf Core.subExpressions rewrite
  where
    rewrite =
        case specialDoubleMode of
            UseYAMLEncoding       -> useYAMLEncoding
            ForbidWithinJSON      -> forbidWithinJSON
            ApproximateWithinJSON -> approximateWithinJSON

    useYAMLEncoding (Core.DoubleLit n)
        | isInfinite n && 0 < n =
            return (Just (Core.TextLit (Core.Chunks [] "inf")))
        | isInfinite n && n < 0 =
            return (Just (Core.TextLit (Core.Chunks [] "-inf")))
        | isNaN n =
            return (Just (Core.TextLit (Core.Chunks [] "nan")))
    useYAMLEncoding _ =
        return Nothing

    forbidWithinJSON (Core.DoubleLit n)
        | isInfinite n || isNaN n =
            Left (SpecialDouble n)
    forbidWithinJSON _ =
        return Nothing

    approximateWithinJSON (Core.DoubleLit n)
        | isInfinite n && n > 0 =
            return (Just (Core.DoubleLit ( 1.7976931348623157e308 :: Double)))
        | isInfinite n && n < 0 =
            return (Just (Core.DoubleLit (-1.7976931348623157e308 :: Double)))
        -- Do nothing for @NaN@, which already encodes to @null@
    approximateWithinJSON _ =
        return Nothing

{-| Convert a piece of Text carrying a Dhall inscription to an equivalent JSON Value

>>> :set -XOverloadedStrings
>>> import Core
>>> Dhall.JSON.codeToValue "(stdin)" "{ a = 1 }"
>>> Object (fromList [("a",Number 1.0)])
-}
codeToValue
  :: Conversion
  -> SpecialDoubleMode
  -> Text  -- ^ Describe the input for the sake of error location.
  -> Text  -- ^ Input text.
  -> IO Value
codeToValue conversion specialDoubleMode name code = do
    parsedExpression <- Core.throws (Dhall.Parser.exprFromText (Data.Text.unpack name) code)

    resolvedExpression <- Dhall.Import.load parsedExpression

    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    let convertedExpression =
            convertToHomogeneousMaps conversion resolvedExpression

    specialDoubleExpression <- Core.throws (handleSpecialDoubles specialDoubleMode convertedExpression)

    case dhallToJSON specialDoubleExpression of
      Left  err  -> Control.Exception.throwIO err
      Right json -> return json

