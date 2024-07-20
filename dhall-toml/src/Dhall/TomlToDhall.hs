{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

{-| This module exports the `tomlToDhall` function for translating a
    TOML syntax tree from @tomland@ to a Dhall syntax tree. For now,
    this package does not have type inference so a Dhall type is needed.

    For converting source code into a Dhall syntax tree see the @dhall@
    package, and for converting the TOML syntax tree to source code see
    the @tomland@ package.

    This module also exports `tomlToDhallMain` which implements the
    @toml-to-dhall@ command which converts TOML source directly into
    Dhall source.

    In theory all TOML objects should be converted but there are some known
    failure cases:
    * Arrays of arrays of objects - not supported by @tomland@
    * Arrays of heterogeneous primitive types - not supported by @tomland@
        * Arrays of objects of different types are allowed (note that this
            requires conversion to a Dhall union)

    TOML bools translate to Dhall @Bool@s:

> $ cat schema.dhall
> { b : Bool }
> $ toml-to-dhall schema.dhall <<< 'b = true'
> { b = True }

    TOML numbers translate to Dhall numbers:

> $ cat schema.dhall
> { n : Natural, d : Double }
> $ toml-to-dhall schema.dhall << EOF
> n = 1
> d = 3.14
> EOF
> { d = 3.14, n = 1}

    TOML text translates to Dhall @Text@:

> $ cat schema.dhall
> { t : Text }
> $ toml-to-dhall schema.dhall << EOF
> t = "Hello!"
> EOF
> { t = "Hello!" }

    TOML arrays and table arrays translate to Dhall @List@:

> $ cat schema.dhall
> { nums : List Natural, tables : List { a : Natural, b : Text } }
> $ toml-to-dhall schema.dhall << EOF
> nums = [1, 2, 3]
>
> [[tables]]
> a = 1
> b = "Hello,"
> [[tables]]
> a = 2
> b = " World!"
> EOF
> { nums = [ 1, 2, 3 ]
> , tables = [ { a = 1, b = "Hello," }, { a = 2, b = " World!" } ]
> }

    Note, [lists of lists of objects](https://github.com/kowainik/tomland/issues/373)
    and [heterogeneous lists](https://github.com/kowainik/tomland/issues/373) are not
    supported by @tomland@ so a paraser error will be returned:

> $ cat schema.dhall
> { list : List (<a : Natural | b : Bool>) }
> $ toml-to-dhall schema.dhall << EOF
> list = [1, true]
> EOF
> toml-to-dhall: invalid TOML:
> 1:12:
>   |
> 1 | list = [1, true]
>   |            ^
> unexpected 't'
> expecting ',', ']', or integer

    Because of this, unions have limited use in lists, but can be used fully
    in tables:

> $ cat schema.dhall
> { list : List (<a : Natural | b : Bool>), item : <a : Natural | b : Bool> }
> $ toml-to-dhall schema.dhall << EOF
> list = [1, 2]
> item = true
> EOF
> { item = < a : Natural | b : Bool >.b True
> , list = [ < a : Natural | b : Bool >.a 1, < a : Natural | b : Bool >.a 2 ]
> }

    TOML tables translate to Dhall records:

> $ cat schema.dhall
> { num : Natural, table : { num1 : Natural, table1 : { num2 : Natural } } }
> $ toml-to-dhall schema.dhall << EOF
> num = 0
>
> [table]
> num1 = 1
>
> [table.table1]
> num2 = 2
> EOF
> { num = 0, table = { num1 = 1, table1.num2 = 2 } }

-}
module Dhall.TomlToDhall
    ( tomlToDhall
    , tomlToDhallMain
    , CompileError
    ) where

import Control.Exception    (Exception(..))
import Data.Bifunctor       (first)
import Data.Either          (rights)
import Data.Foldable        (fold, toList)
import Data.HashMap.Strict  (HashMap)
import Data.List.NonEmpty   (NonEmpty ((:|)))
import Data.Version         (showVersion)
import Data.Void            (Void)
import Dhall.Core           (DhallDouble (..), Expr)
import Dhall.Parser         (Src)
import Dhall.Toml.Utils     (fileToDhall)
import Toml.Parser          (TomlParseError)
import Toml.Type.AnyValue   (AnyValue(..))
import Toml.Type.Key        (Key(..), Piece(..))
import Toml.Type.PrefixTree (PrefixMap, PrefixTree(..))
import Toml.Type.TOML       (TOML)
import Toml.Type.Value      (Value)

import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Sequence        as Seq
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text.IO
import qualified Dhall.Core           as Core
import qualified Dhall.Map            as Map
import qualified Options.Applicative  as Options
import qualified Paths_dhall_toml     as Meta
import qualified Toml.Parser
import qualified Toml.Type.TOML       as TOML
import qualified Toml.Type.Value      as Value

data CompileError
    = Unimplemented String
    | Incompatible (Expr Src Void) Object
    | InvalidToml TomlParseError
    | InternalError String
    | MissingKey String
    deriving (Show)

instance Exception CompileError where
    displayException exception = case exception of
        Unimplemented s ->
            "unimplemented: " <> s
        Incompatible e toml ->
            "incompatible: " <> show e <> " with " <> show toml
        InvalidToml e ->
            "invalid TOML:\n" <> Text.unpack (Toml.Parser.unTomlParseError e)
        InternalError e ->
            "internal error: " <> show e
        MissingKey e ->
            "missing key: " <> show e

tomlToDhall :: Expr Src Void -> TOML -> Either CompileError (Expr Src Void)
tomlToDhall schema toml = objectToDhall (Core.normalize schema) (tomlToObject toml)

valueToDhall
    :: Expr Src Void -> Value t -> Either CompileError (Expr Src Void)
valueToDhall type_ value = case (type_, value) of
    (Core.Bool, Value.Bool a) ->
        Right (Core.BoolLit a)

    (Core.Integer, Value.Integer a) ->
        Right (Core.IntegerLit a)

    (Core.Natural, Value.Integer a) ->
        Right (Core.NaturalLit (fromInteger a))

    (Core.Double, Value.Double a) ->
        Right (Core.DoubleLit (DhallDouble a))

    (Core.Text, Value.Text a) ->
        Right (Core.TextLit (Core.Chunks [] a))

    (_, Value.Zoned _) ->
        Left (Unimplemented "toml time values")

    (_, Value.Local _) ->
        Left (Unimplemented "toml time values")

    (_, Value.Day _) ->
        Left (Unimplemented "toml time values")

    (Core.App Core.List _, Value.Array [] ) ->
        Right (Core.ListLit (Just type_) [])

    (Core.App Core.Optional t, a) -> do
        o <- valueToDhall t a
        return (Core.Some o)

    (Core.App Core.List elementType, Value.Array elements) -> do
        expressions <- mapM (valueToDhall elementType) elements
        return (Core.ListLit Nothing (Seq.fromList expressions))

    -- TODO: allow different types of matching (ex. first, strict, none)
    -- currently we just pick the first enum that matches
    (Core.Union m, _) -> do
        let f key maybeAlternativeType = case maybeAlternativeType of
                Just alternativeType -> do
                    expression <- valueToDhall alternativeType value
                    return (Core.App (Core.Field type_ (Core.makeFieldSelection key)) expression)
                Nothing -> case value of
                    Value.Text a | a == key ->
                        return (Core.Field type_ (Core.makeFieldSelection a))
                    _ -> Left (Incompatible type_ (Prim (AnyValue value)))

        case rights (toList (Map.mapWithKey f m)) of
            []    -> Left (Incompatible type_ (Prim (AnyValue value)))
            x : _ -> Right x

    _ ->
        Left (Incompatible type_ (Prim (AnyValue value)))

-- TODO: keep track of the path for more helpful error messages
objectToDhall :: Expr Src Void -> Object -> Either CompileError (Expr Src Void)
objectToDhall type_ object = case (type_, object) of
    (_, Invalid) -> Left (InternalError "invalid object")

    -- TODO: allow different types of matching (ex. first, strict, none)
    -- currently we just pick the first enum that matches
    (Core.Union m, _) -> do
        let f key maybeAlternativeType = case maybeAlternativeType of
                Just alternativeType -> do
                    expression <- objectToDhall alternativeType object
                    return (Core.App (Core.Field type_ (Core.makeFieldSelection key)) expression)
                Nothing -> case object of
                    Prim (AnyValue (Value.Text a)) | a == key ->
                        return (Core.Field type_ (Core.makeFieldSelection a))
                    _ -> Left (Incompatible type_ object)

        case rights (toList (Map.mapWithKey f m)) of
            []    -> Left (Incompatible type_ object)
            x : _ -> Right x

    (Core.Record record, Table table) -> do
        let process key fieldType
                | Just nestedObject <- HashMap.lookup (Piece key) table =
                    objectToDhall fieldType nestedObject
                | Core.App Core.Optional innerType <- fieldType =
                    Right (Core.App Core.None innerType)
                | Core.App Core.List _ <- fieldType =
                    Right (Core.ListLit (Just fieldType) [])
                | otherwise =
                    Left (MissingKey (Text.unpack key))

        expressions <- Map.traverseWithKey process (fmap Core.recordFieldValue record)

        return (Core.RecordLit (fmap Core.makeRecordField expressions))

    (Core.App Core.List (Core.Record [("mapKey", Core.recordFieldValue -> Core.Text), ("mapValue", Core.recordFieldValue -> valueType)]), Table table) -> do
        hashMap <- traverse (objectToDhall valueType) table

        let expressions = Seq.fromList do
                (Piece key, value) <- HashMap.toList hashMap

                let newKey =
                        Core.makeRecordField (Core.TextLit (Core.Chunks [] key))

                let newValue = Core.makeRecordField value

                pure (Core.RecordLit [("mapKey", newKey), ("mapValue", newValue)])

        let listType = if Seq.null expressions then Just type_ else Nothing

        return (Core.ListLit listType expressions)

    (Core.App Core.List t, Array []) ->
        Right (Core.ListLit (Just t) [])

    (Core.App Core.List t, Array elements) -> do
        expressions <- mapM (objectToDhall t) elements
        return (Core.ListLit Nothing (Seq.fromList expressions))

    (_, Prim (AnyValue value)) ->
        valueToDhall type_ value

    (_, obj) ->
        Left (Incompatible type_ obj)

-- | An intermediate object created from a 'TOML' before an 'Expr'.
--   It does two things, firstly joining the tomlPairs, tomlTables,
--   and tomlTableArrays parts of the TOML. Second, it turns the dense
--   paths (ex. a.b.c = 1) into sparse paths (ex. a = { b = { c = 1 }}).
data Object
    = Prim AnyValue
    | Array [Object]
    | Table (HashMap Piece Object)
    | Invalid
    deriving (Show)

instance Semigroup Object where
    Table ls <> Table rs = Table (ls <> rs)
    -- this shouldn't happen because tomland has already verified correctness
    -- of the toml object
    _ <> _ = Invalid

instance Monoid Object where
    mempty = Table HashMap.empty

-- | Creates an arbitrarily nested object
sparseObject :: Key -> Object -> Object
sparseObject (Key (piece :| [])) value =
    Table (HashMap.singleton piece value)
sparseObject (Key (piece :| piece' : pieces)) value =
    Table (HashMap.singleton piece (sparseObject (Key (piece' :| pieces)) value))

tablesToObject :: PrefixMap TOML -> Object
tablesToObject = fold . map prefixTreeToObject . HashMap.elems

prefixTreeToObject :: PrefixTree TOML -> Object
prefixTreeToObject (Leaf key toml) =
    sparseObject key (tomlToObject toml)
prefixTreeToObject (Branch prefix _ toml) =
    sparseObject prefix (tablesToObject toml)

tomlToObject :: TOML -> Object
tomlToObject = pairs <> tables <> tableArrays
  where
    pairs =
          fold
        . HashMap.mapWithKey sparseObject
        . fmap Prim
        . TOML.tomlPairs

    tables =
          fold
        . map prefixTreeToObject
        . HashMap.elems
        . TOML.tomlTables

    tableArrays =
          fold
        . HashMap.mapWithKey sparseObject
        . fmap (Array . fmap tomlToObject . toList)
        . TOML.tomlTableArrays

data Options = Options
    { input :: Maybe FilePath
    , output :: Maybe FilePath
    , schemaFile :: FilePath
    }

parserInfo :: Options.ParserInfo Options
parserInfo = Options.info
    (Options.helper <*> versionOption <*> optionsParser)
    (Options.fullDesc <> Options.progDesc "Convert TOML to Dhall")
  where
    versionOption =
        Options.infoOption (showVersion Meta.version)
            (Options.long "version" <> Options.help "Display version")

    optionsParser = do
        input <- (Options.optional . Options.strOption)
            (  Options.long "file"
            <> Options.help "Read TOML from file instead of standard input"
            <> Options.metavar "FILE"
            <> Options.action "file"
            )
        output <- (Options.optional . Options.strOption)
            (  Options.long "output"
            <> Options.help "Write Dhall to a file instead of standard output"
            <> Options.metavar "FILE"
            <> Options.action "file"
            )
        schemaFile <- Options.strArgument
            (  Options.help "Path to Dhall schema file"
            <> Options.action "file"
            <> Options.metavar "SCHEMA"
            )
        pure Options {..}

tomlToDhallMain :: IO ()
tomlToDhallMain = do
    Options{..} <- Options.execParser parserInfo

    inputText <- case input of
        Just file -> Text.IO.readFile file
        Nothing   -> Text.IO.getContents

    toml <- Core.throws (first InvalidToml (Toml.Parser.parse inputText))

    schema <- fileToDhall schemaFile

    dhall <- Core.throws (tomlToDhall schema toml)

    let outputText = Core.pretty dhall

    case output of
        Just file -> Text.IO.writeFile file outputText
        Nothing   -> Text.IO.putStrLn outputText
