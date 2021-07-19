module Dhall.DhallToToml
    ( dhallToTomlMain
    , dhallToToml
    ) where

import Control.Monad        (foldM)
import Control.Exception    (Exception, throwIO)
import Data.Foldable        (toList)
import Data.List.NonEmpty   (NonEmpty((:|)))
import Data.Text            (Text)
import Data.Void            (Void)
import Dhall.Core           (Expr, DhallDouble(..))
import Dhall.Toml.Utils     (inputToDhall)
import Dhall.TypeCheck
import Toml.Type.TOML       (TOML)
import Toml.Type.Key        (Piece(Piece), Key(Key, unKey))
import Toml.Type.Printer    (pretty)

import qualified Data.Bifunctor       as Bifunctor
import qualified Data.Sequence        as Seq
import qualified Data.Text.IO         as Text.IO
import qualified Dhall.Map            as Map
import qualified Dhall.Core           as Core
import qualified Toml.Type.TOML       as Toml.TOML
import qualified Toml.Type.Value      as Toml.Value
import qualified Toml.Type.AnyValue   as Toml.AnyValue


data CompileError
    = Unimplemented String
    | Unsupported (Expr Void Void)
    -- | tomland does not support records in multi-dimensional arrays, though it
    --   is allowed by the spec
    | UnsupportedArray (Expr Void Void)
    | NotARecord (Expr Void Void)
    -- | the latest TOML spec, v1.0.0 allows this but tomland has not
    --   implemented it yet
    --   NOTE: the only way to get this error is through unions
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
    r <- assertRecordLit (Core.normalize e0)
    toTomlTable r

assertRecordLit :: Expr Void Void -> Either CompileError (Map.Map Text (Core.RecordField Void Void))
assertRecordLit (Core.RecordLit r) = Right r
assertRecordLit e                  = Left $ NotARecord e

toTomlTable :: Map.Map Text (Core.RecordField Void Void) -> Either CompileError TOML
toTomlTable r = foldM (toTomlRecordFold []) (mempty :: TOML) (Map.toList r)

toTomlRecordFold :: [Piece] -> TOML -> (Text, Core.RecordField Void Void) -> Either CompileError TOML
toTomlRecordFold curKey toml' (key', val) = toToml toml' newKey (Core.recordFieldValue val)
    where
        append :: [Piece] -> Piece -> NonEmpty Piece
        append []     y = y :| []
        append (x:xs) y = x :| xs ++ [y]
        newKey = Key $ append curKey $ Piece key'

toToml :: TOML -> Key -> Expr Void Void -> Either CompileError TOML
toToml toml key expr  = case expr of
    Core.BoolLit a -> return $ insertPrim (Toml.Value.Bool a)
    Core.NaturalLit a -> return $ insertPrim (Toml.Value.Integer $ toInteger a)
    Core.DoubleLit (DhallDouble a) -> return $ insertPrim (Toml.Value.Double a)
    Core.TextLit (Core.Chunks [] a) -> return $ insertPrim (Toml.Value.Text a)
    -- empty union alternative like < A | B >.A
    Core.Field (Core.Union _) (Core.FieldSelection _ a _) -> return $ insertPrim (Toml.Value.Text a)
    -- union alternative with type like < A : Natural | B>.A 1
    Core.App (Core.Field (Core.Union _) _) a -> toToml toml key a
    Core.ListLit _ a -> case toList a of
        -- empty array
        [] -> return $ insertPrim (Toml.Value.Array [])
        -- array of table
        record@(Core.RecordLit _) : records -> do
            tables' <- case mapM assertRecordLit (record :| records)  of
                Right x -> mapM toTomlTable x
                Left (NotARecord e) -> Left (HeterogeneousArray e)
                Left x -> Left x
            return $ Toml.TOML.insertTableArrays key tables' toml
        -- inline array
        a' -> do
            anyList <- mapM toAny a'
            let arrayEither = Toml.AnyValue.toMArray anyList
            array <- Bifunctor.first (const $ HeterogeneousArray expr) arrayEither
            return $ insertPrim array
    Core.RecordLit r ->
        let
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
            then foldM (toTomlRecordFold $ toList $ unKey key) toml (Map.toList nested)
            else do
                -- the order here is important, at least for testing, because
                -- the PrefixMap inside TOML is dependent on insert order
                inlinePairs <- foldM (toTomlRecordFold []) mempty      (Map.toList inline)
                nestedPairs <- foldM (toTomlRecordFold []) inlinePairs (Map.toList nested)
                return $ Toml.TOML.insertTable key nestedPairs toml
    _ -> Left $ Unsupported expr
    where
        insertPrim :: Toml.Value.Value a -> TOML
        insertPrim val = Toml.TOML.insertKeyVal key val toml

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

dhallToTomlMain :: IO ()
dhallToTomlMain = do
    resolvedExpression <- inputToDhall
    toml <- case dhallToToml resolvedExpression of
        Left err -> throwIO err
        Right toml -> return toml
    Text.IO.putStrLn $ pretty toml


