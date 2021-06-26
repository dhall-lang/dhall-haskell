
-- TODO: split module into Dhall.DhallToToml and Dhall.TomlToDhall modules
module Dhall.Toml
    ( tomlToDhallMain
    , tomlToDhall
    , dhallToTomlMain
    , dhallToToml
    ) where

import Control.Monad     (foldM)
import Control.Exception (Exception, throwIO)
import Data.Foldable     (toList)
import Data.Void         (Void)
import Dhall.Core        (Expr, DhallDouble(..))
import Dhall.Parser      (Src)
import Toml.Type.TOML    (TOML)
import Toml.Type.Key     (Piece(Piece), Key(Key))
import Toml.Type.Printer (pretty)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence      as Seq
import qualified Data.Text.IO       as Text.IO
import qualified Dhall.Map          as Map
import qualified Dhall.Core         as Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Toml.Type.TOML     as Toml.TOML
import qualified Toml.Type.Value    as Toml.Value
import qualified Toml.Type.AnyValue as Toml.AnyValue


data CompileError
    = Unimplemented String
    | Unsupported (Expr Void Void)
    -- tomland does not support records in multi-dimensional arrays, though it
    -- is alloed by the spec
    | UnsupportedArray (Expr Void Void)
    | NotARecord (Expr Void Void)
    -- the latest TOML spec, v1.0.0 allows this but tomland has not
    -- implemented it yet
    -- NOTE: the only way to get this error is through enums
    | HeterogeneousArray (Expr Void Void)

instance Show CompileError where
    show (Unimplemented s) = "unimplemented: " ++ s
    show (Unsupported e) = "unsupported: " ++ show e
    show (UnsupportedArray e) = "records cannot be nested in multi-dimentional arrays: " ++ show e
    show (NotARecord e) = "The root object converted to TOML must be a record, got " ++ show e
    show (HeterogeneousArray e) = "heterogeneous arrays are not currently supported " ++ show e

instance Exception CompileError

dhallToToml :: Expr s Void -> Either CompileError TOML
dhallToToml e0 = do
    let norm = Core.normalize e0
    _ <- assertRecordLit norm
    toToml (mempty :: TOML) [] norm
    where
        assertRecordLit (Core.RecordLit r) = Right r
        assertRecordLit e                  = Left $ NotARecord e

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right c) = Right c

-- | A helper function for dhallToToml. It recursively adds the values in
--   the Expr to the TOML. It has an invariant that key can be null iff
--   Expr is a RecordLit. This aligns with how a TOML document must be a table,
--   and bare values cannot be represented
toToml :: TOML -> [Piece] -> Expr Void Void -> Either CompileError TOML
toToml toml key expr  = case expr of
    Core.BoolLit a -> return $ insertPrim (Toml.Value.Bool a)
    Core.NaturalLit a -> return $ insertPrim (Toml.Value.Integer $ toInteger a)
    Core.DoubleLit (DhallDouble a) -> return $ insertPrim (Toml.Value.Double a)
    Core.TextLit (Core.Chunks [] a) -> return $ insertPrim (Toml.Value.Text a)
    Core.ListLit _ a -> case Seq.lookup 0 a of
        -- empty array
        Nothing -> return $ insertPrim (Toml.Value.Array [])
        -- array of table
        Just (Core.RecordLit _) -> do
            tables <- mapM (toToml mempty []) $ toList a
            let tables' = NonEmpty.fromList tables
            let key' = Key $ NonEmpty.fromList key
            return $ Toml.TOML.insertTableArrays key' tables' toml
        -- inline array
        Just _ -> do
            anyList <- mapM toAny $ toList a
            let arrayEither = Toml.AnyValue.toMArray anyList
            array <- mapLeft (const $ HeterogeneousArray expr) arrayEither
            return $ insertPrim array
    Core.RecordLit r ->
        let
            f curKey toml' (key', val) = toToml toml' (curKey ++ [Piece key']) (Core.recordFieldValue val)
        in
            if null key -- at the top level, we can't have a table
            then foldM (f []) toml (Map.toList r)
            else let
                (inline, nested) = Map.partition (isInline . Core.recordFieldValue) r
            in
                if null inline
                -- if the table doesn't have inline elements, don't register
                -- the table, only its non-inlined children. Ex:
                -- [a] # bad
                --   [b]
                --     c = 1
                -- [a.b] # good
                --   c = 1
                then foldM (f key) toml (Map.toList nested)
                else do
                    -- the order here is important, at least for testing, because
                    -- the PrefixMap inside TOML is dependent on insert order
                    inlinePairs <- foldM (f []) mempty      (Map.toList inline)
                    nestedPairs <- foldM (f []) inlinePairs (Map.toList nested)
                    return $ Toml.TOML.insertTable (Key $ NonEmpty.fromList key) nestedPairs toml
    _ -> Left $ Unsupported expr
    where
        -- insert a value at the current key to the TOML, note that
        -- the current key cannot be empty. This is true assuming
        -- the root call to toToml is always called with a RecordLit
        insertPrim :: Toml.Value.Value a -> TOML
        insertPrim val = Toml.TOML.insertKeyVal (Key $ NonEmpty.fromList key) val toml

        -- checks if the value should be represented as an inline key/value
        -- pair. Elements that are inlined are those that do not have a
        -- [header] or [[header]]. One edge case is tables within multiple
        -- arrays, though not currently supported by tomland, can only
        -- be represented as inline tables.
        isInline v = case v of
            Core.BoolLit _    -> True
            Core.NaturalLit _ -> True
            Core.DoubleLit _  -> True
            Core.TextLit _    -> True
            Core.ListLit _ s  -> case Seq.lookup 0 s of
                Nothing                  -> True
                Just (Core.BoolLit _)    -> True
                Just (Core.NaturalLit _) -> True
                Just (Core.DoubleLit _)  -> True
                Just (Core.TextLit _)    -> True
                Just (Core.ListLit _ _)  -> True
                _                        -> False
            _ -> False

        rightAny = Right . Toml.AnyValue.AnyValue

        -- toAny is a helper function for making lists so it returns a list
        -- specific error, in particular tomland's inability to represent
        -- tables in multi-dimensional arrays
        toAny :: Expr Void Void -> Either CompileError Toml.AnyValue.AnyValue
        toAny e = case e of
            Core.BoolLit x                  -> rightAny $ Toml.Value.Bool x
            Core.NaturalLit x               -> rightAny $ Toml.Value.Integer $ toInteger x
            Core.DoubleLit (DhallDouble x)  -> rightAny $ Toml.Value.Double x
            Core.TextLit (Core.Chunks [] x) -> rightAny $ Toml.Value.Text x
            Core.ListLit _ x                -> do
                anyList <- mapM toAny $ toList x
                case Toml.AnyValue.toMArray anyList of
                    Right x' -> rightAny x'
                    Left _ -> Left $ HeterogeneousArray expr
            Core.RecordLit _ -> Left $ UnsupportedArray e
            _ -> Left $ Unsupported e

type ExprX = Expr Src Void

tomlToDhall :: ExprX -> TOML -> Either CompileError ExprX
tomlToDhall _ _ = Left $ Unimplemented "toml -> dhall"

dhallToTomlMain :: IO ()
dhallToTomlMain = do
    text <- Text.IO.getContents
    parsedExpression <- Core.throws (Dhall.Parser.exprFromText "(input)" text)
    resolvedExpression <- Dhall.Import.load parsedExpression
    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)
    toml <- case dhallToToml resolvedExpression of
        Left err -> throwIO err
        Right toml -> return toml
    Text.IO.putStrLn $ pretty toml


tomlToDhallMain :: IO ()
tomlToDhallMain = putStrLn "not implemented"

