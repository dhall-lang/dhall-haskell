{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module Dhall.TomlToDhall
    ( tomlToDhall
    , tomlToDhallMain
    , CompileError
    ) where

import Control.Exception    (Exception, throwIO)
import Data.Either          (rights)
import Data.Foldable        (toList, foldl')
import Data.List.NonEmpty   (NonEmpty((:|)))
import Data.Text            (Text)
import Data.Void            (Void)
import Dhall.Core           (Expr, DhallDouble(..))
import Dhall.Toml.Utils     (fileToDhall)
import Dhall.Parser         (Src)
import Toml.Parser          (TomlParseError)
import Toml.Type.AnyValue   (AnyValue(AnyValue))
import Toml.Type.Value      (Value)
import Toml.Type.TOML       (TOML)
import Toml.Type.Key        (Piece(Piece), Key(Key))
import Toml.Type.PrefixTree (PrefixTree)

import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Sequence        as Seq
import qualified Data.Text
import qualified Data.Text.IO         as Text.IO
import qualified Dhall.Map            as Map
import qualified Dhall.Core           as Core
import qualified Toml.Parser
import qualified Toml.Type.TOML       as Toml.TOML
import qualified Toml.Type.PrefixTree as Toml.PrefixTree
import qualified Toml.Type.Value      as Value
import qualified Toml.Type.AnyValue   as Toml.AnyValue
import qualified System.Environment

data CompileError
    = Unimplemented String
    | Incompatible (Expr Src Void) Object
    | InvalidToml TomlParseError
    | InternalError String
    | MissingKey String

instance Show CompileError where
    show (Unimplemented s) = "unimplemented: " ++ s
    show (Incompatible e toml) = "incompatible: " ++ (show e) ++ " with " ++ (show toml)
    show (InvalidToml e) = "invalid TOML: " ++ show e
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
    _ -> Left $ Incompatible exprType (Prim (AnyValue v))

-- TODO: keep track of the path for more helpful error messages
toDhall :: Expr Src Void -> Object -> Either CompileError (Expr Src Void)
toDhall exprType value = case (exprType, value) of
    (_,                    Invalid)  -> Left $ InternalError "invalid object"

    -- TODO: allow different types of matching (ex. first, strict, none)
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

tomlToDhallMain :: IO ()
tomlToDhallMain = do
    text <- Text.IO.getContents
    toml <- case Toml.Parser.parse text of
        Left tomlErr -> throwIO (InvalidToml tomlErr)
        Right toml -> return toml
    args <- System.Environment.getArgs
    schemaFile <- case args of
        [] -> fail "schema not provided"
        schemaFile:[] -> return schemaFile
        _ -> fail "too many agrgs"
    schema <- fileToDhall schemaFile
    dhall <- case tomlToDhall schema toml of
        Left err -> throwIO err
        Right dhall -> return dhall
    Text.IO.putStrLn $ Core.pretty dhall

