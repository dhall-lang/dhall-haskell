{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-| Convert JSON data to Dhall in one of two ways:

    * By default, the conversion will make a best-effort at inferring the
      corresponding Dhall type

    * Optionally, you can specify an expected Dhall type necessary to make the
      translation unambiguous.

    Either way, if you supply the generated Dhall result to @dhall-to-json@ you
    should get back the original JSON.

    Only a subset of Dhall types are supported when converting from JSON:

    * @Bool@
    * @Natural@
    * @Integer@
    * @Double@
    * @Text@
    * @List@
    * @Optional@
    * unions
    * records
    * @Prelude.Type.Map@
    * @Prelude.Type.JSON@ - You can always convert JSON data to this type as a
      last resort if you don't know the schema in advance.

    You can use this code as a library (this module) or as an executable
    named @json-to-dhall@, which is used in the examples below.

    By default the @json-to-dhall@ executable attempts to infer the
    appropriate Dhall type from the JSON data, like this:

> $ json-to-dhall <<< 1
> 1

    ... but you can also provide an explicit schema on the command line if you
    prefer a slightly different Dhall type which still represents the same JSON
    value:

> $ json-to-dhall Integer <<< 1
> +1

    You can also get the best of both worlds by using the @type@ subcommand to
    infer the schema:

> $ json-to-dhall type <<< '[ "up", "down" ]' | tee schema.dhall
> List Text

    ... and then edit the @./schema.dhall@ file to better match the type you
    intended, such as:

> $ $EDITOR schema.dhall
> $ cat ./schema.dhall
> List < up | down >

    ... and then use the edited schema for subsequent conversions:

> $ json-to-dhall ./schema.dhall <<< '[ "up", "down" ]'
> [ < down | up >.up, < down | up >.down ]

== Primitive types

    JSON @Bool@s translate to Dhall bools:

> $ json-to-dhall <<< 'true'
> True
> $ json-to-dhall <<< 'false'
> False

    JSON numbers translate to Dhall numbers:

> $ json-to-dhall <<< 2
> 2
> $ json-to-dhall <<< -2
> -2
> $ json-to-dhall <<< -2.1
> -2.1
> $ json-to-dhall Natural <<< 2
> 2
> $ json-to-dhall Integer <<< 2
> +2
> $ json-to-dhall Double <<< 2
> 2.0

    JSON text corresponds to Dhall @Text@ by default:

> $ json-to-dhall <<< '"foo bar"'
> "foo bar"

    ... but you can also decode text into a more structured enum, too, if you
    provide an explicit schema:

> $ json-to-dhall '< A | B >' <<< '"A"'
> < A | B >.A

== Lists and records

    Dhall @List@s correspond to JSON lists:

> $ json-to-dhall <<< '[ 1, 2, 3 ]'
> [ 1, 2, 3 ]

    You can even decode an empty JSON list to Dhall:

> $ json-to-dhall <<< '[]'
> [] : List <>

    ... which will infer the empty @\<\>@ type if there are no other constraints
    on the type.  If you provide an explicit type annotation then the conversion
    will use that instead:

> $ json-to-dhall 'List Natural' <<< '[]'
> [] : List Natural

    Dhall records correspond to JSON records:

> $ json-to-dhall <<< '{ "foo": [ 1, 2, 3 ] }'
> { foo = [ 1, 2, 3 ] }

    If you specify a schema with additional @Optional@ fields then they will be
    @None@ if absent:

> $ json-to-dhall '{ foo : List Natural, bar : Optional Bool }' <<< '{ "foo": [ 1, 2, 3 ] }'
> { bar = None Bool, foo = [ 1, 2, 3 ] }

    ... and @Some@ if present:

> $ json-to-dhall '{ foo : List Natural, bar : Optional Bool }' <<< '{ "foo": [ 1, 2, 3 ], "bar": true }'
> { bar = Some True, foo = [ 1, 2, 3 ] }

    If you specify a schema with too few fields, then the behavior is
    configurable.  By default, the conversion will reject extra fields:

> $ json-to-dhall '{ foo : List Natural }' <<< '{ "foo": [ 1, 2, 3 ], "bar": true }'
>
> Error: Key(s) bar present in the JSON object but not in the expected Dhall record type. This is not allowed unless you enable the --records-loose flag:
>
> Expected Dhall type:
> { foo : List Natural }
>
> JSON:
> {
>     "foo": [
>         1,
>         2,
>         3
>     ],
>     "bar": true
> }

  ... as the error message suggests, extra fields are ignored if you enable the
  @--records-loose@ flag.

> $ json-to-dhall --records-loose '{ foo : List Natural }' <<< '{ "foo": [ 1, 2, 3 ], "bar": true }'
> { foo = [ 1, 2, 3 ] }

    You can convert JSON key-value arrays to Dhall records, but only if you
    supply an explicit Dhall type:

> $ json-to-dhall '{ a : Natural, b : Text }' <<< '[ { "key": "a", "value": 1 }, { "key": "b", "value": "asdf" } ]'
> { a = 1, b = "asdf" }

    You can also disable this behavior using the @--no-keyval-arrays@:

> $ json-to-dhall --no-keyval-arrays '{ a : Natural, b : Text }' <<< '[ { "key": "a", "value": 1 }, { "key": "b", "value": "asdf" } ]'
> Error: JSON (key-value) arrays cannot be converted to Dhall records under --no-keyval-arrays flag:

    You can also convert JSON records to Dhall @Map@s, but only if you supply an
    explicit schema:

> $ json-to-dhall 'List { mapKey : Text, mapValue : Text }' <<< '{ "foo": "bar" }'
> toMap { foo = "bar" }

    The map keys can even be union types instead of `Text`:

> $ json-to-dhall 'List { mapKey : < A | B >, mapValue : Natural }' <<< '{ "A": 1, "B": 2 }'
> [ { mapKey = < A | B >.A, mapValue = 1 }, { mapKey = < A | B >.B, mapValue = 2 } ]

    You can similarly disable this feature using @--no-keyval-maps@:

> $ json-to-dhall --no-keyval-maps 'List { mapKey : Text, mapValue : Text }' <<< '{ "foo": "bar" }'
> Error: Homogeneous JSON map objects cannot be converted to Dhall association lists under --no-keyval-arrays flag

    If your schema is a record with a `List` field and omit that field in the JSON,
    you'll get an error:

> $ json-to-dhall  '{ a : List Natural }' <<< '{}'
>
>
> Error: Key a, expected by Dhall type:
> List Natural
> is not present in JSON object:
> {}

    You can use the @--omissible-lists@ option to default to an empty list in this case

> $ json-to-dhall --omissible-lists  '{ a : List Natural }' <<< '{}'
> { a = [] : List Natural }

== Optional values and unions

    JSON @null@ values correspond to @Optional@ Dhall values:

> $ json-to-dhall <<< 'null'
> None <>

    ... and the schema inference logic will automatically wrap other values in
    @Optional@ to ensure that the types line up:

> $ json-to-dhall <<< '[ 1, null ]'
> [ Some 1, None Natural ]

    A field that might be absent also corresponds to an @Optional@ type:

> $ json-to-dhall <<< '[ { "x": 1 }, { "x": 2, "y": true } ]'
> [ { x = 1, y = None Bool }, { x = 2, y = Some True } ]

    For Dhall union types the correct value will be based on matching the type
    of JSON expression if you give an explicit type:

> $ json-to-dhall 'List < Left : Text | Right : Integer >' <<< '[1, "bar"]'
> [ < Left : Text | Right : Integer >.Right +1
> , < Left : Text | Right : Integer >.Left "bar"
> ]

    Also, the schema inference logic will still infer a union anyway in order
    to reconcile simple types:

> $ json-to-dhall <<< '[ 1, true ]'
> [ < Bool : Bool | Natural : Natural >.Natural 1
> , < Bool : Bool | Natural : Natural >.Bool True
> ]

    In presence of multiple potential matches, the first will be selected by
    default:

> $ json-to-dhall '{foo : < Left : Text | Middle : Text | Right : Integer >}' <<< '{ "foo": "bar"}'
> { foo = < Left : Text | Middle : Text | Right : Integer >.Left "bar" }

    This will result in error if @--unions-strict@ flag is used, with the list
    of alternative matches being reported (as a Dhall list)

> $ json-to-dhall --unions-strict '{foo : < Left : Text | Middle : Text | Right : Integer >}' <<< '{ "foo": "bar"}'
> Error: More than one union component type matches JSON value
> ...
> Possible matches:
> < Left : Text | Middle : Text | Right : Integer >.Left "bar"
> --------
> < Left : Text | Middle : Text | Right : Integer >.Middle "bar"

== Weakly-typed JSON

If you don't know the JSON's schema in advance, you can decode into the most
general schema possible:

> $ cat ./schema.dhall
> https://prelude.dhall-lang.org/JSON/Type

> $ json-to-dhall ./schema.dhall <<< '[ { "foo": null, "bar": [ 1.0, true ] } ]'
>   λ(JSON : Type)
> → λ(string : Text → JSON)
> → λ(number : Double → JSON)
> → λ(object : List { mapKey : Text, mapValue : JSON } → JSON)
> → λ(array : List JSON → JSON)
> → λ(bool : Bool → JSON)
> → λ(null : JSON)
> → array
>   [ object
>     ( toMap
>         { bar = array [ number 1.0, bool True ]
>         , foo = null
>         }
>     )
>   ]

You can also mix and match JSON fields whose schemas are known or unknown:

> $ cat ./mixed.dhall
> List
> { foo : Optional Natural
> , bar : https://prelude.dhall-lang.org/JSON/Type
> }

> $ json-to-dhall ./mixed.dhall <<< '[ { "foo": null, "bar": [ 1.0, true ] } ]'
> [ { bar =
>         λ(JSON : Type)
>       → λ(string : Text → JSON)
>       → λ(number : Double → JSON)
>       → λ(object : List { mapKey : Text, mapValue : JSON } → JSON)
>       → λ(array : List JSON → JSON)
>       → λ(bool : Bool → JSON)
>       → λ(null : JSON)
>       → array [ number 1.0, bool True ]
>   , foo =
>       None Natural
>   }
> ]

    The schema inference algorithm will also infer this schema of last resort
    when unifying a simple type with a record or a list:

> $ json-to-dhall <<< '[ 1, [] ]'
> [ λ(JSON : Type) →
>   λ ( json
>     : { array : List JSON → JSON
>       , bool : Bool → JSON
>       , double : Double → JSON
>       , integer : Integer → JSON
>       , null : JSON
>       , object : List { mapKey : Text, mapValue : JSON } → JSON
>       , string : Text → JSON
>       }
>     ) →
>     json.integer +1
> , λ(JSON : Type) →
>   λ ( json
>     : { array : List JSON → JSON
>       , bool : Bool → JSON
>       , double : Double → JSON
>       , integer : Integer → JSON
>       , null : JSON
>       , object : List { mapKey : Text, mapValue : JSON } → JSON
>       , string : Text → JSON
>       }
>     ) →
>     json.array ([] : List JSON)
> ]

-}

module Dhall.JSONToDhall (
    -- * JSON to Dhall
      parseConversion
    , Conversion(..)
    , defaultConversion
    , UnionConv(..)
    , resolveSchemaExpr
    , typeCheckSchemaExpr
    , dhallFromJSON

    -- * Schema inference
    , Schema(..)
    , RecordSchema(..)
    , UnionSchema(..)
    , inferSchema
    , schemaToDhallType

    -- * Exceptions
    , CompileError(..)
    , showCompileError
    ) where

import Control.Applicative      ((<|>))
import Control.Exception        (Exception, throwIO)
import Control.Monad.Catch      (MonadCatch, throwM)
import Data.Aeson               (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Either              (rights)
import Data.Foldable            (toList)
import Data.List                ((\\))
import Data.Monoid              (Any (..))
import Data.Scientific          (floatingOrInteger, toRealFloat)
import Data.Text                (Text)
import Data.Void                (Void)
import Dhall.Core               (Chunks (..), DhallDouble (..), Expr (App))
import Dhall.JSON.Util          (pattern FA, pattern V)
import Dhall.Parser             (Src)
import Options.Applicative      (Parser)

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Foldable              as Foldable
import qualified Data.Map
import qualified Data.Map.Merge.Lazy        as Data.Map.Merge
import qualified Data.Sequence              as Seq
import qualified Data.String
import qualified Data.Text                  as Text
import qualified Data.Vector                as Vector
import qualified Dhall.Core                 as D
import qualified Dhall.Import
import qualified Dhall.JSON.Compat          as JSON.Compat
import qualified Dhall.Lint                 as Lint
import qualified Dhall.Map                  as Map
import qualified Dhall.Optics               as Optics
import qualified Dhall.Parser
import qualified Dhall.TypeCheck            as D
import qualified Options.Applicative        as O

-- ---------------
-- Command options
-- ---------------

-- | Standard parser for options related to the conversion method
parseConversion :: Parser Conversion
parseConversion = Conversion <$> parseStrict
                             <*> parseKVArr
                             <*> parseKVMap
                             <*> parseUnion
                             <*> parseOmissibleLists
  where
    parseStrict =
            O.flag' True
            (  O.long "records-strict"
            <> O.help "Fail if any JSON fields are missing from the expected Dhall type"
            )
        <|> O.flag' False
            (  O.long "records-loose"
            <> O.help "Tolerate JSON fields not present within the expected Dhall type"
            )
        <|> pure True

    parseKVArr  =  O.switch
                (  O.long "no-keyval-arrays"
                <> O.help "Disable conversion of key-value arrays to records"
                )
    parseKVMap  =  O.switch
                (  O.long "no-keyval-maps"
                <> O.help "Disable conversion of homogeneous map objects to association lists"
                )
    parseOmissibleLists = O.switch
                          ( O.long "omissible-lists"
                          <> O.help "Tolerate missing list values, they are assumed empty"
                          )

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

-- ----------
-- Conversion
-- ----------

-- | JSON-to-dhall translation options
data Conversion = Conversion
    { strictRecs     :: Bool
    , noKeyValArr    :: Bool
    , noKeyValMap    :: Bool
    , unions         :: UnionConv
    , omissibleLists :: Bool
    } deriving Show

data UnionConv = UFirst | UNone | UStrict deriving (Show, Read, Eq)

-- | Default conversion options
defaultConversion :: Conversion
defaultConversion = Conversion
    { strictRecs     = False
    , noKeyValArr    = False
    , noKeyValMap    = False
    , unions         = UFirst
    , omissibleLists = False
    }

-- | The 'Expr' type concretization used throughout this module
type ExprX = Expr Src Void

-- | Parse schema code and resolve imports
resolveSchemaExpr :: Text  -- ^ type code (schema)
                  -> IO ExprX
resolveSchemaExpr code = do
    parsedExpression <-
      case Dhall.Parser.exprFromText "\n\ESC[1;31mSCHEMA\ESC[0m" code of
        Left  err              -> throwIO err
        Right parsedExpression -> return parsedExpression
    Dhall.Import.load parsedExpression

{-| Check that the Dhall type expression actually has type 'Type'
>>> :set -XOverloadedStrings
>>> import Dhall.Core

>>> typeCheckSchemaExpr id =<< resolveSchemaExpr "List Natural"
App List Natural

>>> typeCheckSchemaExpr id =<< resolveSchemaExpr "+1"
*** Exception:
Error: Schema expression is successfully parsed but has Dhall type:
Integer
Expected Dhall type: Type
Parsed expression: +1
-}
typeCheckSchemaExpr :: (Exception e, MonadCatch m)
                    => (CompileError -> e) -> ExprX -> m ExprX
typeCheckSchemaExpr compileException expr =
  case D.typeOf expr of -- check if the expression has type
    Left  err -> throwM . compileException $ TypeError err
    Right t   -> case t of -- check if the expression has type Type
      D.Const D.Type -> return expr
      _              -> throwM . compileException $ BadDhallType t expr

keyValMay :: Value -> Maybe (Text, Value)
keyValMay (Aeson.Object o) = do
     Aeson.String k <- JSON.Compat.lookupObject "key" o
     v <- JSON.Compat.lookupObject "value" o
     return (k, v)
keyValMay _ = Nothing

{-| Given a JSON `Value`, make a best-effort guess of what the matching Dhall
    type should be

    This is used by @{json,yaml}-to-dhall@ if the user does not supply a schema
    on the command line
-}
inferSchema :: Value -> Schema
inferSchema (Aeson.Object m) =
    let convertMap = Data.Map.fromDistinctAscList . JSON.Compat.mapToAscList

    in (Record . RecordSchema . convertMap) (fmap inferSchema m)
inferSchema (Aeson.Array xs) =
    List (Foldable.foldMap inferSchema xs)
inferSchema (Aeson.String _) =
    Text
inferSchema (Aeson.Number n) =
    case floatingOrInteger n of
        Left (_ :: Double) -> Double
        Right (integer :: Integer)
            | 0 <= integer -> Natural
            | otherwise    -> Integer
inferSchema (Aeson.Bool _) =
    Bool
inferSchema Aeson.Null =
    Optional mempty

-- | Aeson record type that `inferSchema` can infer
newtype RecordSchema =
    RecordSchema { getRecordSchema :: Data.Map.Map Text Schema }

instance Semigroup RecordSchema where
    RecordSchema l <> RecordSchema r = RecordSchema m
      where
        -- The reason this is not @Just (Optional s)@ is to avoid creating a
        -- double `Optional` wrapper when unifying a @null@ field with an
        -- absent field.
        onMissing _ s = Just (s <> Optional mempty)

        m = Data.Map.Merge.merge
                (Data.Map.Merge.mapMaybeMissing onMissing)
                (Data.Map.Merge.mapMaybeMissing onMissing)
                (Data.Map.Merge.zipWithMatched (\_ -> (<>)))
                l
                r

recordSchemaToDhallType :: RecordSchema -> Expr s a
recordSchemaToDhallType (RecordSchema m) =
    D.Record (Map.fromList (Data.Map.toList (fmap (D.makeRecordField . schemaToDhallType) m)))

{-| `inferSchema` will never infer a union type with more than one numeric
    alternative

    Instead, the most general alternative type will be preferred, which this
    type tracks
-}
data UnionNumber
    = UnionAbsent
    -- ^ The union type does not have a numeric alternative
    | UnionNatural
    -- ^ The union type has a @Natural@ alternative
    | UnionInteger
    -- ^ The union type has an @Integer@ alternative
    | UnionDouble
    -- ^ The union type has a @Double@ alternative
    deriving (Bounded, Eq, Ord)

-- | Unify two numeric alternative types by preferring the most general type
instance Semigroup UnionNumber where
    (<>) = max

instance Monoid UnionNumber where
    mempty = minBound

unionNumberToAlternatives :: UnionNumber -> [ (Text, Maybe (Expr s a)) ]
unionNumberToAlternatives UnionAbsent  = []
unionNumberToAlternatives UnionNatural = [ ("Natural", Just D.Natural) ]
unionNumberToAlternatives UnionInteger = [ ("Integer", Just D.Integer) ]
unionNumberToAlternatives UnionDouble  = [ ("Double" , Just D.Double ) ]

{-| A union type that `inferSchema` can infer

    This type will have at most three alternatives:

    * A @Bool@ alternative
    * Either a @Natural@, @Integer@, or @Double@ alternative
    * A @Text@ alternative

    These alternatives will always use the same names and types when we convert
    back to a Dhall type, so we only need to keep track of whether or not each
    alternative is present.

    We only store simple types inside of a union since we treat any attempt to
    unify a simple type with a complex type as a strong indication that the
    user intended for the schema to be `ArbitraryJSON`.
-}
data UnionSchema = UnionSchema
    { bool :: Any
    -- ^ `True` if the union has a @Bool@ alternative
    , number :: UnionNumber
    -- ^ Up to one numeric alternative
    , text :: Any
    -- ^ `True` if the union has a @Text@ alternative
    } deriving (Eq)

unionSchemaToDhallType :: UnionSchema -> Expr s a
unionSchemaToDhallType UnionSchema{..} = D.Union (Map.fromList alternatives)
  where
    alternatives =
            (if getAny bool then [ ("Bool", Just D.Bool) ] else [])
        <>  unionNumberToAlternatives number
        <>  (if getAny text then [ ("Text", Just D.Text) ] else [])

-- | Unify two union types by combining their alternatives
instance Semigroup UnionSchema where
    UnionSchema boolL numberL textL <> UnionSchema boolR numberR textR =
        UnionSchema{..}
      where
        bool = boolL <> boolR

        number = numberL <> numberR

        text = textL <> textR

instance Monoid UnionSchema where
    mempty = UnionSchema{..}
      where
        bool = mempty

        number = mempty

        text = mempty

{-| A `Schema` is a subset of the `Expr` type representing all possible
    Dhall types that `inferSchema` could potentially return
-}
data Schema
    = Bool
    | Natural
    | Integer
    | Double
    | Text
    | List Schema
    | Optional Schema
    | Record RecordSchema
    | Union UnionSchema
    | ArbitraryJSON

-- | (`<>`) unifies two schemas
instance Semigroup Schema where
    -- `ArbitraryJSON` subsumes every other type
    ArbitraryJSON <> _ = ArbitraryJSON
    _ <> ArbitraryJSON = ArbitraryJSON

    -- Simple types unify with themselves
    Bool    <> Bool    = Bool
    Text    <> Text    = Text
    Natural <> Natural = Natural
    Integer <> Integer = Integer
    Double  <> Double  = Double

    -- Complex types unify with themselves
    Record   l <> Record   r = Record   (l <> r)
    List     l <> List     r = List     (l <> r)
    Union    l <> Union    r = Union    (l <> r)
    Optional l <> Optional r = Optional (l <> r)

    -- Numeric types unify on the most general numeric type
    Natural <> Integer = Integer
    Integer <> Natural = Integer
    Natural <> Double  = Double
    Integer <> Double  = Double
    Double  <> Natural = Double
    Double  <> Integer = Double

    -- Unifying two different simple types produces a union
    Bool    <> Natural = Union mempty{ bool = Any True, number = UnionNatural }
    Bool    <> Integer = Union mempty{ bool = Any True, number = UnionInteger }
    Bool    <> Double  = Union mempty{ bool = Any True, number = UnionDouble }
    Bool    <> Text    = Union mempty{ bool = Any True, text = Any True }
    Natural <> Bool    = Union mempty{ bool = Any True, number = UnionNatural }
    Natural <> Text    = Union mempty{ number = UnionNatural, text = Any True }
    Integer <> Bool    = Union mempty{ bool = Any True, number = UnionInteger }
    Integer <> Text    = Union mempty{ number = UnionInteger, text = Any True }
    Double  <> Bool    = Union mempty{ bool = Any True, number = UnionDouble }
    Double  <> Text    = Union mempty{ number = UnionDouble, text = Any True }
    Text    <> Bool    = Union mempty{ bool = Any True, text = Any True }
    Text    <> Natural = Union mempty{ number = UnionNatural, text = Any True }
    Text    <> Integer = Union mempty{ number = UnionInteger, text = Any True }
    Text    <> Double  = Union mempty{ number = UnionDouble, text = Any True }

    -- The empty union type is the identity of unification
    Union l <> r | l == mempty = r
    l <> Union r | r == mempty = l

    -- Unifying a simple type with a union adds the simple type as yet another
    -- alternative
    Bool    <> Union r = Union (mempty{ bool   = Any True } <> r)
    Natural <> Union r = Union (mempty{ number = UnionNatural } <> r)
    Integer <> Union r = Union (mempty{ number = UnionInteger } <> r)
    Double  <> Union r = Union (mempty{ number = UnionDouble} <> r)
    Text    <> Union r = Union (mempty{ text   = Any True } <> r)
    Union l <> Bool    = Union (l <> mempty{ bool   = Any True })
    Union l <> Natural = Union (l <> mempty{ number = UnionNatural })
    Union l <> Integer = Union (l <> mempty{ number = UnionInteger })
    Union l <> Double  = Union (l <> mempty{ number = UnionDouble })
    Union l <> Text    = Union (l <> mempty{ text   = Any True })

    -- All of the remaining cases are for unifying simple types with
    -- complex types.  The only such case that can be sensibly unified is for
    -- `Optional`

    -- `Optional` subsumes every type other than `ArbitraryJSON`
    Optional l <> r = Optional (l <> r)
    l <> Optional r = Optional (l <> r)

    -- For all other cases, a simple type cannot be unified with a complex
    -- type, so fall back to `ArbitraryJSON`
    --
    -- This is equivalent to:
    --
    --     _ <> _ = ArbitraryJSON
    --
    -- ... but more explicit, in order to minimize the chance of ignoring an
    -- important case by accident.
    List _   <> _        = ArbitraryJSON
    _        <> List _   = ArbitraryJSON
    Record _ <> _        = ArbitraryJSON
    _        <> Record _ = ArbitraryJSON

instance Monoid Schema where
    mempty = Union mempty

-- | Convert a `Schema` to the corresponding Dhall type
schemaToDhallType :: Schema -> Expr s a
schemaToDhallType Bool = D.Bool
schemaToDhallType Natural = D.Natural
schemaToDhallType Integer = D.Integer
schemaToDhallType Double = D.Double
schemaToDhallType Text = D.Text
schemaToDhallType (List a) = D.App D.List (schemaToDhallType a)
schemaToDhallType (Optional a) = D.App D.Optional (schemaToDhallType a)
schemaToDhallType (Record r) = recordSchemaToDhallType r
schemaToDhallType (Union u) = unionSchemaToDhallType u
schemaToDhallType ArbitraryJSON =
    D.Pi mempty "_" (D.Const D.Type)
        (D.Pi mempty "_"
            (D.Record
                [ ("array" , D.makeRecordField $ D.Pi mempty "_" (D.App D.List (V 0)) (V 1))
                , ("bool"  , D.makeRecordField $ D.Pi mempty "_" D.Bool (V 1))
                , ("double", D.makeRecordField $ D.Pi mempty "_" D.Double (V 1))
                , ("integer", D.makeRecordField $ D.Pi mempty "_" D.Integer (V 1))
                , ("null"  , D.makeRecordField $ V 0)
                , ("object", D.makeRecordField $
                    D.Pi mempty "_" (D.App D.List (D.Record
                        [ ("mapKey", D.makeRecordField D.Text)
                        , ("mapValue", D.makeRecordField $ V 0)
                        ])) (V 1))
                , ("string", D.makeRecordField $ D.Pi mempty "_" D.Text (V 1))
                ]
            )
            (V 1)
        )

{-| The main conversion function. Traversing\/zipping Dhall /type/ and Aeson value trees together to produce a Dhall /term/ tree, given 'Conversion' options:

>>> :set -XOverloadedStrings
>>> import qualified Dhall.Core as D
>>> import qualified Dhall.Map as Map
>>> import qualified Data.Aeson as Aeson
>>> import qualified Data.HashMap.Strict as HM

>>> s = D.Record (Map.fromList [("foo", D.Integer)])
>>> v = Aeson.Object (HM.fromList [("foo", Aeson.Number 1)])
>>> dhallFromJSON defaultConversion s v
Right (RecordLit (fromList [("foo",IntegerLit 1)]))

-}
dhallFromJSON
  :: Conversion -> ExprX -> Value -> Either CompileError ExprX
dhallFromJSON (Conversion {..}) expressionType =
    fmap (Optics.rewriteOf D.subExpressions Lint.useToMap) . loop [] (D.alphaNormalize (D.normalize expressionType))
  where
    loop :: Aeson.Types.JSONPath -> ExprX -> Aeson.Value -> Either CompileError ExprX
    -- any ~> Union
    loop jsonPath t@(D.Union tm) v = do
      let f key maybeType =
            case maybeType of
              Just _type -> do
                expression <- loop jsonPath _type v

                return (D.App (D.Field t $ FA key) expression)

              Nothing ->
                case v of
                    Aeson.String text | key == text ->
                        return (D.Field t $ FA key)
                    _ ->
                        Left (Mismatch t v jsonPath)

      case (unions, rights (toList (Map.mapWithKey f tm))) of
        (UNone  , _         ) -> Left (ContainsUnion t)
        (UStrict, xs@(_:_:_)) -> Left (UndecidableUnion t v xs)
        (_      , [ ]       ) -> Left (Mismatch t v jsonPath)
        (UFirst , x:_       ) -> Right x
        (UStrict, [x]       ) -> Right x

    -- object ~> Record
    loop jsonPath (D.Record r) v@(Aeson.Object o)
        | extraKeys <- JSON.Compat.objectKeys o \\ Map.keys r
        , strictRecs && not (null extraKeys)
        = Left (UnhandledKeys extraKeys (D.Record r) v jsonPath)
        | otherwise
        = let f :: Text -> ExprX -> Either CompileError ExprX
              f k t | Just value <- JSON.Compat.lookupObject k o
                    = loop (Aeson.Types.Key (JSON.Compat.textToKey k) : jsonPath) t value
                    | App D.Optional t' <- t
                    = Right (App D.None t')
                    | App D.List _ <- t
                    , omissibleLists
                    = Right (D.ListLit (Just t) [])
                    | otherwise
                    = Left (MissingKey k t v jsonPath)
           in D.RecordLit . fmap D.makeRecordField <$> Map.traverseWithKey f (D.recordFieldValue <$> r)

    -- key-value list ~> Record
    loop jsonPath t@(D.Record _) v@(Aeson.Array a)
        | not noKeyValArr
        , os :: [Value] <- toList a
        , Just kvs <- traverse keyValMay os
        = loop jsonPath t (Aeson.Object $ JSON.Compat.objectFromList kvs)
        | noKeyValArr
        = Left (NoKeyValArray t v)
        | otherwise
        = Left (Mismatch t v jsonPath)

    -- object ~> List (key, value)
    loop jsonPath t@(App D.List (D.Record r)) v@(Aeson.Object o)
        | not noKeyValMap
        , ["mapKey", "mapValue"] == Map.keys r
        , Just mapKey   <- D.recordFieldValue <$> Map.lookup "mapKey" r
        , Just mapValue <- D.recordFieldValue <$> Map.lookup "mapValue" r
        = do
          keyExprMap <- JSON.Compat.traverseObjectWithKey (\k child -> loop (Aeson.Types.Key k : jsonPath) mapValue child) o

          toKey <-
              case mapKey of
                  D.Text    -> return $ D.TextLit . Chunks []
                  D.Union _ -> return $ D.Field mapKey . FA
                  _         -> Left (Mismatch t v jsonPath)

          let f :: (Text, ExprX) -> ExprX
              f (key, val) = D.RecordLit $ D.makeRecordField <$> Map.fromList
                  [ ("mapKey"  , toKey key)
                  , ("mapValue", val)
                  ]

          let records =
                (fmap f . Seq.fromList . JSON.Compat.mapToAscList) keyExprMap

          let typeAnn = if null o then Just t else Nothing

          return (D.ListLit typeAnn records)
        | noKeyValMap
        = Left (NoKeyValMap t v)
        | otherwise
        = Left (Mismatch t v jsonPath)

    -- array ~> List
    loop jsonPath (App D.List t) (Aeson.Array a)
        = let f :: [ExprX] -> ExprX
              f es = D.ListLit
                       (if null es then Just (App D.List t) else Nothing)
                       (Seq.fromList es)
           in f <$> traverse (\(idx, val) -> loop (Aeson.Types.Index idx : jsonPath) t val) (zip [0..] $ toList a)

    -- null ~> List
    loop jsonPath t@(App D.List _) Aeson.Null
        = if omissibleLists
          then Right (D.ListLit (Just t) [])
          else Left (Mismatch t Aeson.Null jsonPath)

    -- number ~> Integer
    loop jsonPath D.Integer (Aeson.Number x)
        | Right n <- floatingOrInteger x :: Either Double Integer
        = Right (D.IntegerLit n)
        | otherwise
        = Left (Mismatch D.Integer (Aeson.Number x) jsonPath)

    -- number ~> Natural
    loop jsonPath D.Natural (Aeson.Number x)
        | Right n <- floatingOrInteger x :: Either Double Integer
        , n >= 0
        = Right (D.NaturalLit (fromInteger n))
        | otherwise
        = Left (Mismatch D.Natural (Aeson.Number x) jsonPath)

    -- number ~> Double
    loop _ D.Double (Aeson.Number x)
        = Right (D.DoubleLit $ DhallDouble $ toRealFloat x)

    -- string ~> Text
    loop _ D.Text (Aeson.String t)
        = Right (D.TextLit (Chunks [] t))

    -- bool ~> Bool
    loop _ D.Bool (Aeson.Bool t)
        = Right (D.BoolLit t)

    -- null ~> Optional
    loop _ (App D.Optional expr) Aeson.Null
        = Right $ App D.None expr

    -- value ~> Optional
    loop jsonPath (App D.Optional expr) value
        = D.Some <$> loop jsonPath expr value

    -- Arbitrary JSON ~> https://prelude.dhall-lang.org/JSON/Type (< v13.0.0)
    loop
      _
      (D.Pi _ _ (D.Const D.Type)
          (D.Pi _ _
              (D.Record
                  [ ("array" , D.recordFieldValue -> D.Pi _ _ (D.App D.List (V 0)) (V 1))
                  , ("bool"  , D.recordFieldValue -> D.Pi _ _ D.Bool (V 1))
                  , ("null"  , D.recordFieldValue -> V 0)
                  , ("number", D.recordFieldValue -> D.Pi _ _ D.Double (V 1))
                  , ("object", D.recordFieldValue ->
                      D.Pi _ _ (D.App D.List (D.Record
                      [ ("mapKey", D.recordFieldValue -> D.Text)
                      , ("mapValue", D.recordFieldValue -> V 0)
                      ])) (V 1))
                  , ("string", D.recordFieldValue -> D.Pi _ _ D.Text (V 1))
                  ]
              )
              (V 1)
          )
      )
      value = do
          let outer (Aeson.Object o) =
                  let inner (key, val) =
                          D.RecordLit
                              [ ("mapKey"  , D.makeRecordField $ D.TextLit (D.Chunks [] key))
                              , ("mapValue", D.makeRecordField $ outer val                  )
                              ]

                      elements =
                          Seq.fromList
                              (fmap inner
                                  (JSON.Compat.mapToAscList o)
                              )

                      elementType
                          | null elements =
                              Just (D.App D.List (D.Record
                                [ ("mapKey", D.makeRecordField D.Text)
                                , ("mapValue", D.makeRecordField "JSON")
                                ]))
                          | otherwise =
                              Nothing

                      keyValues = D.ListLit elementType elements

                  in  D.App (D.Field "json" $ FA "object") keyValues
              outer (Aeson.Array a) =
                  let elements = Seq.fromList (fmap outer (Vector.toList a))

                      elementType
                          | null elements = Just (D.App D.List "JSON")
                          | otherwise     = Nothing

                  in  D.App (D.Field "json" $ FA "array") (D.ListLit elementType elements)
              outer (Aeson.String s) =
                  D.App (D.Field "json" $ FA "string") (D.TextLit (D.Chunks [] s))
              outer (Aeson.Number n) =
                  D.App (D.Field "json" $ FA "number") (D.DoubleLit (DhallDouble (toRealFloat n)))
              outer (Aeson.Bool b) =
                  D.App (D.Field "json" $ FA "bool") (D.BoolLit b)
              outer Aeson.Null =
                  D.Field "json" $ FA "null"

          let result =
                D.Lam mempty (D.makeFunctionBinding "JSON" (D.Const D.Type))
                    (D.Lam mempty (D.makeFunctionBinding "json"
                        (D.Record
                            [ ("array" , D.makeRecordField $ D.Pi mempty "_" (D.App D.List "JSON") "JSON")
                            , ("bool"  , D.makeRecordField $ D.Pi mempty "_" D.Bool "JSON")
                            , ("null"  , D.makeRecordField "JSON")
                            , ("number", D.makeRecordField $ D.Pi mempty "_" D.Double "JSON")
                            , ("object", D.makeRecordField $
                                D.Pi mempty "_" (D.App D.List (D.Record
                                    [ ("mapKey", D.makeRecordField D.Text)
                                    , ("mapValue", D.makeRecordField "JSON")
                                    ])) "JSON")
                            , ("string", D.makeRecordField $ D.Pi mempty "_" D.Text "JSON")
                            ]
                        ))
                        (outer value)
                    )

          return result

    -- Arbitrary JSON ~> https://prelude.dhall-lang.org/JSON/Type (v13.0.0 <=)
    loop
      _
      (D.Pi _ _ (D.Const D.Type)
          (D.Pi _ _
              (D.Record
                  [ ("array" , D.recordFieldValue -> D.Pi _ _ (D.App D.List (V 0)) (V 1))
                  , ("bool"  , D.recordFieldValue -> D.Pi _ _ D.Bool (V 1))
                  , ("double", D.recordFieldValue -> D.Pi _ _ D.Double (V 1))
                  , ("integer", D.recordFieldValue -> D.Pi _ _ D.Integer (V 1))
                  , ("null"  , D.recordFieldValue -> V 0)
                  , ("object", D.recordFieldValue ->
                      D.Pi _ _ (D.App D.List (D.Record
                        [ ("mapKey", D.recordFieldValue -> D.Text)
                        , ("mapValue", D.recordFieldValue -> V 0)
                        ])) (V 1))
                  , ("string", D.recordFieldValue -> D.Pi _ _ D.Text (V 1))
                  ]
              )
              (V 1)
          )
      )
      value = do
          let outer (Aeson.Object o) =
                  let inner (key, val) =
                          D.RecordLit
                              [ ("mapKey"  , D.makeRecordField $ D.TextLit (D.Chunks [] key))
                              , ("mapValue", D.makeRecordField $ outer val                  )
                              ]

                      elements =
                          Seq.fromList
                              (fmap inner
                                  (JSON.Compat.mapToAscList o)
                              )

                      elementType
                          | null elements =
                              Just (D.App D.List (D.Record
                                [ ("mapKey", D.makeRecordField D.Text)
                                , ("mapValue", D.makeRecordField "JSON") ]))
                          | otherwise =
                              Nothing

                      keyValues = D.ListLit elementType elements

                  in  D.App (D.Field "json" (FA "object")) keyValues
              outer (Aeson.Array a) =
                  let elements = Seq.fromList (fmap outer (Vector.toList a))

                      elementType
                          | null elements = Just (D.App D.List "JSON")
                          | otherwise     = Nothing

                  in  D.App (D.Field "json" (FA "array")) (D.ListLit elementType elements)
              outer (Aeson.String s) =
                  D.App (D.Field "json" (FA "string")) (D.TextLit (D.Chunks [] s))
              outer (Aeson.Number n) =
                  case floatingOrInteger n of
                      Left floating -> D.App (D.Field "json" (FA "double")) (D.DoubleLit (DhallDouble floating))
                      Right integer -> D.App (D.Field "json" (FA "integer")) (D.IntegerLit integer)
              outer (Aeson.Bool b) =
                  D.App (D.Field "json" (FA "bool")) (D.BoolLit b)
              outer Aeson.Null =
                  D.Field "json" (FA "null")

          let result =
                D.Lam mempty (D.makeFunctionBinding "JSON" (D.Const D.Type))
                    (D.Lam mempty (D.makeFunctionBinding "json"
                        (D.Record
                            [ ("array" , D.makeRecordField $ D.Pi mempty "_" (D.App D.List "JSON") "JSON")
                            , ("bool"  , D.makeRecordField $ D.Pi mempty "_" D.Bool "JSON")
                            , ("double", D.makeRecordField $ D.Pi mempty "_" D.Double "JSON")
                            , ("integer", D.makeRecordField $ D.Pi mempty "_" D.Integer "JSON")
                            , ("null"  , D.makeRecordField "JSON")
                            , ("object", D.makeRecordField $ D.Pi mempty "_"
                                (D.App D.List (D.Record
                                    [ ("mapKey", D.makeRecordField D.Text)
                                    , ("mapValue", D.makeRecordField "JSON")])) "JSON")
                            , ("string", D.makeRecordField $ D.Pi mempty "_" D.Text "JSON")
                            ]
                        ))
                        (outer value)
                    )

          return result

    -- fail
    loop jsonPath expr value
        = Left (Mismatch expr value jsonPath)


-- ----------
-- EXCEPTIONS
-- ----------

red, purple, green
    :: (Semigroup a, Data.String.IsString a) => a -> a
red    s = "\ESC[1;31m" <> s <> "\ESC[0m" -- bold
purple s = "\ESC[1;35m" <> s <> "\ESC[0m" -- bold
green  s = "\ESC[0;32m" <> s <> "\ESC[0m" -- plain

showExpr :: ExprX   -> String
showExpr dhall = Text.unpack (D.pretty dhall)

showJSON :: Value -> String
showJSON value = BSL8.unpack (encodePretty value)

data CompileError
  -- Dhall shema
  = TypeError (D.TypeError Src Void)
  | BadDhallType
      ExprX -- Expression type
      ExprX -- Whole expression
  -- generic mismatch (fallback)
  | Mismatch
      ExprX   -- Dhall expression
      Value -- Aeson value
      Aeson.Types.JSONPath -- JSON Path to the error
  -- record specific
  | MissingKey     Text  ExprX Value Aeson.Types.JSONPath
  | UnhandledKeys [Text] ExprX Value Aeson.Types.JSONPath
  | NoKeyValArray        ExprX Value
  | NoKeyValMap          ExprX Value
  -- union specific
  | ContainsUnion        ExprX
  | UndecidableUnion     ExprX Value [ExprX]

instance Show CompileError where
    show = showCompileError "JSON" showJSON

instance Exception CompileError

showCompileError :: String -> (Value -> String) -> CompileError -> String
showCompileError format showValue = let prefix = red "\nError: "
          in \case
    TypeError e -> show e

    BadDhallType t e -> prefix
      <> "Schema expression is successfully parsed but has Dhall type:\n"
      <> showExpr t <> "\nExpected Dhall type: Type"
      <> "\nParsed expression: "
      <> showExpr e <> "\n"

    ContainsUnion e -> prefix
      <> "Dhall type expression contains union type:\n"
      <> showExpr e <> "\nwhile it is forbidden by option "
      <> green "--unions-none\n"

    UndecidableUnion e v xs -> prefix
      <> "More than one union component type matches " <> format <> " value"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n\nPossible matches:\n\n" -- Showing all the allowed matches
      <> Text.unpack (Text.intercalate sep $ D.pretty <$> xs)
        where sep = red "\n--------\n" :: Text

    Mismatch e v jsonPath -> prefix
      <> showJsonPath jsonPath <> ": Dhall type expression and " <> format <> " value do not match:"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n"

    MissingKey k e v jsonPath -> prefix
      <> showJsonPath jsonPath <> ": Key " <> purple (Text.unpack k) <> ", expected by Dhall type:\n"
      <> showExpr e
      <> "\nis not present in " <> format <> " object:\n"
      <> showValue v <> "\n"

    UnhandledKeys ks e v jsonPath -> prefix
      <> showJsonPath jsonPath <> ": Key(s) " <> purple (Text.unpack (Text.intercalate ", " ks))
      <> " present in the " <> format <> " object but not in the expected Dhall"
      <> " record type. This is not allowed unless you enable the "
      <> green "--records-loose" <> " flag:"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n"

    NoKeyValArray e v -> prefix
      <> "" <> format <> " (key-value) arrays cannot be converted to Dhall records under "
      <> green "--no-keyval-arrays" <> " flag"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n"

    NoKeyValMap e v -> prefix
      <> "Homogeneous " <> format <> " map objects cannot be converted to Dhall association lists under "
      <> green "--no-keyval-maps" <> " flag"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n"

showJsonPath :: Aeson.Types.JSONPath -> String
showJsonPath = Aeson.Types.formatPath . reverse
