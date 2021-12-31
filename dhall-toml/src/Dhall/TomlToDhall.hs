{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Exception    (Exception, throwIO)
import Data.Either          (rights)
import Data.Foldable        (foldl', toList)
import Data.List.NonEmpty   (NonEmpty ((:|)))
import Data.Text            (Text)
import Data.Version         (showVersion)
import Data.Void            (Void)
import Dhall.Core           (DhallDouble (..), Expr)
import Dhall.Parser         (Src)
import Dhall.Toml.Utils     (fileToDhall)
import Toml.Parser          (TomlParseError)
import Toml.Type.AnyValue   (AnyValue (AnyValue))
import Toml.Type.Key        (Key (Key), Piece (Piece))
import Toml.Type.PrefixTree (PrefixTree)
import Toml.Type.TOML       (TOML)
import Toml.Type.Value      (Value)

import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Sequence        as Seq
import qualified Data.Text
import qualified Data.Text.IO         as Text.IO
import qualified Dhall.Core           as Core
import qualified Dhall.Map            as Map
import qualified Options.Applicative  as OA
import qualified Paths_dhall_toml     as Meta
import qualified Toml.Parser
import qualified Toml.Type.AnyValue   as Toml.AnyValue
import qualified Toml.Type.PrefixTree as Toml.PrefixTree
import qualified Toml.Type.TOML       as Toml.TOML
import qualified Toml.Type.Value      as Value

data CompileError
    = Unimplemented String
    | Incompatible (Expr Src Void) Object
    | InvalidToml TomlParseError
    | InternalError String
    | MissingKey String

instance Show CompileError where
    show (Unimplemented s) = "unimplemented: " ++ s
    show (Incompatible e toml) = "incompatible: " ++ (show e) ++ " with " ++ (show toml)
    show (InvalidToml e) = "invalid TOML:\n" ++ (Data.Text.unpack $ Toml.Parser.unTomlParseError e)
    show (InternalError e) = "internal error: " ++ show e
    show (MissingKey e) = "missing key: " ++ show e

instance Exception CompileError

tomlToDhall :: Expr Src Void -> TOML -> Either CompileError (Expr Src Void)
tomlToDhall schema toml = toDhall (Core.normalize schema) (tomlToObject toml)

tomlValueToDhall :: Expr Src Void -> Value t -> Either CompileError (Expr Src Void)
tomlValueToDhall exprType v = case (exprType, v) of
    (Core.Bool                , Value.Bool a   ) -> Right $ Core.BoolLit a
    (Core.Natural             , Value.Integer a) -> Right $ Core.NaturalLit $ fromInteger a
    (Core.Double              , Value.Double a ) -> Right $ Core.DoubleLit $ DhallDouble a
    (Core.Text                , Value.Text a   ) -> Right $ Core.TextLit $ Core.Chunks [] a
    (_                        , Value.Zoned _  ) -> Left $ Unimplemented "toml time values"
    (_                        , Value.Local _  ) -> Left $ Unimplemented "toml time values"
    (_                        , Value.Day _    ) -> Left $ Unimplemented "toml time values"
    (t@(Core.App Core.List _) , Value.Array [] ) -> Right $ Core.ListLit (Just t) []
    (Core.App Core.Optional t , a              ) -> do
        o <- tomlValueToDhall t a
        return $ Core.Some o
    (Core.App Core.List t     , Value.Array a  ) -> do
        l <- mapM (tomlValueToDhall t) a
        return $ Core.ListLit Nothing (Seq.fromList l)

    -- TODO: allow different types of matching (ex. first, strict, none)
    -- currently we just pick the first enum that matches
    (Core.Union m        , _)        -> let
        f key maybeType = case maybeType of
            Just ty -> do
                expr <- tomlValueToDhall ty v
                return $ Core.App (Core.Field exprType $ Core.makeFieldSelection key) expr
            Nothing -> case v of
                Value.Text a | a == key ->
                    return $ Core.Field exprType (Core.makeFieldSelection a)
                _ -> Left $ Incompatible exprType (Prim (AnyValue v))

        in case rights (toList (Map.mapWithKey f m)) of
            []  -> Left $ Incompatible exprType (Prim (AnyValue v))
            x:_ -> Right $ x

    _ -> Left $ Incompatible exprType (Prim (AnyValue v))

-- TODO: keep track of the path for more helpful error messages
toDhall :: Expr Src Void -> Object -> Either CompileError (Expr Src Void)
toDhall exprType value = case (exprType, value) of
    (_,                    Invalid)  -> Left $ InternalError "invalid object"

    -- TODO: allow different types of matching (ex. first, strict, none)
    -- currently we just pick the first enum that matches
    (Core.Union m        , _)        -> let
        f key maybeType = case maybeType of
            Just ty -> do
                expr <- toDhall ty value
                return $ Core.App (Core.Field exprType $ Core.makeFieldSelection key) expr
            Nothing -> case value of
                Prim (AnyValue (Value.Text a)) | a == key ->
                    return $ Core.Field exprType (Core.makeFieldSelection a)
                _ -> Left $ Incompatible exprType value

        in case rights (toList (Map.mapWithKey f m)) of
            []  -> Left $ Incompatible exprType value
            x:_ -> Right $ x

    (Core.App Core.List t, Array []) -> Right $ Core.ListLit (Just t) []

    (Core.App Core.List t, Array a) -> do
        l <- mapM (toDhall t) a
        return $ Core.ListLit Nothing (Seq.fromList l)

    (Core.Record r, Table t) -> let
        f :: Text -> (Expr Src Void) -> Either CompileError (Expr Src Void)
        f k ty | Just val <- HashMap.lookup (Piece k) t = toDhall ty val
               | Core.App Core.Optional ty' <- ty = Right $ (Core.App Core.None ty')
               | Core.App Core.List _ <- ty = Right $ Core.ListLit (Just ty) []
               | otherwise = Left $ MissingKey $ Data.Text.unpack k
        in do
            values <- Map.traverseWithKey f (Core.recordFieldValue <$> r)
            return $ Core.RecordLit (Core.makeRecordField <$> values)

    (_, Prim (AnyValue v)) -> tomlValueToDhall exprType v

    (ty, obj) -> Left $ Incompatible ty obj


-- | An intermediate object created from a 'TOML' before an 'Expr'.
--   It does two things, firstly joining the tomlPairs, tomlTables,
--   and tomlTableArrays parts of the TOML. Second, it turns the dense
--   paths (ex. a.b.c = 1) into sparse paths (ex. a = { b = { c = 1 }}).
data Object
    = Prim Toml.AnyValue.AnyValue
    | Array [Object]
    | Table (HashMap.HashMap Piece Object)
    | Invalid
    deriving (Show)

instance Semigroup Object where
    (Table ls) <> (Table rs) = Table (ls <> rs)
    -- this shouldn't happen because tomland has already verified correctness
    -- of the toml object
    _ <> _ = Invalid

-- | Creates an arbitrarily nested object
sparseObject :: Key -> Object -> Object
sparseObject (Key (piece :| [])) value = Table $ HashMap.singleton piece value
sparseObject (Key (piece :| rest:rest')) value
    = Table $ HashMap.singleton piece (sparseObject (Key $ rest :| rest') value)

pairsToObject :: HashMap.HashMap Key Toml.AnyValue.AnyValue -> Object
pairsToObject pairs
    = foldl' (<>) (Table HashMap.empty)
    $ HashMap.mapWithKey sparseObject
    $ fmap Prim pairs

tablesToObject :: Toml.PrefixTree.PrefixMap TOML -> Object
tablesToObject tables
    = foldl' (<>) (Table HashMap.empty)
    $ map prefixTreeToObject
    $ HashMap.elems tables

prefixTreeToObject :: PrefixTree TOML -> Object
prefixTreeToObject (Toml.PrefixTree.Leaf key toml)
    = sparseObject key (tomlToObject toml)
prefixTreeToObject (Toml.PrefixTree.Branch prefix _ toml)
    = sparseObject prefix (tablesToObject toml)

tableArraysToObject :: HashMap.HashMap Key (NonEmpty TOML) -> Object
tableArraysToObject arrays
    = foldl' (<>) (Table HashMap.empty)
    $ HashMap.mapWithKey sparseObject
    $ fmap (Array . fmap tomlToObject . toList)  arrays

tomlToObject :: TOML -> Object
tomlToObject toml = pairs <> tables <> tableArrays
    where
        pairs = pairsToObject $ Toml.TOML.tomlPairs toml
        tables = tablesToObject $ Toml.TOML.tomlTables toml
        tableArrays = tableArraysToObject $ Toml.TOML.tomlTableArrays toml

data Options = Options
    { input :: Maybe FilePath
    , output :: Maybe FilePath
    , schemaFile :: FilePath
    }

parserInfo :: OA.ParserInfo Options
parserInfo = OA.info
    (OA.helper <*> versionOption <*> optionsParser)
    (OA.fullDesc <> OA.progDesc "Convert TOML to Dhall")
  where
    versionOption = OA.infoOption (showVersion Meta.version) $
        OA.long "version" <> OA.help "Display version"
    optionsParser = do
        input <- OA.optional . OA.strOption $
               OA.long "file"
            <> OA.help "Read TOML from file instead of standard input"
            <> fileOpts
        output <- OA.optional . OA.strOption $
               OA.long "output"
            <> OA.help "Write Dhall to a file instead of standard output"
            <> fileOpts
        schemaFile <- OA.strArgument $
               OA.help "Path to Dhall schema file"
            <> OA.action "file"
            <> OA.metavar "SCHEMA"
        pure Options {..}
    fileOpts = OA.metavar "FILE" <> OA.action "file"

tomlToDhallMain :: IO ()
tomlToDhallMain = do
    Options {..} <- OA.execParser parserInfo
    text <- maybe Text.IO.getContents Text.IO.readFile input
    toml <- case Toml.Parser.parse text of
        Left tomlErr -> throwIO (InvalidToml tomlErr)
        Right toml -> return toml
    schema <- fileToDhall schemaFile
    dhall <- case tomlToDhall schema toml of
        Left err -> throwIO err
        Right dhall -> return dhall
    maybe Text.IO.putStrLn Text.IO.writeFile output $ Core.pretty dhall
