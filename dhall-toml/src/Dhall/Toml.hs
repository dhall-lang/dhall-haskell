
-- TODO: split module into Dhall.DhallToToml and Dhall.TomlToDhall modules
module Dhall.Toml
    ( tomlToDhallMain
    , tomlToDhall
    , dhallToTomlMain
    , dhallToToml
    ) where

import Control.Monad     (foldM)
import Control.Exception (Exception)
import Data.Foldable     (foldl')
import Data.Text         (Text, unpack)
import Data.Void         (Void)
import Dhall.Map         (Map)
import Dhall.Core        (Expr, DhallDouble(..))
import Dhall.Parser      (Src)
import Toml.Type.TOML    (TOML)
import Toml.Type.Key     (Piece(Piece), Key(Key))
import Toml.Type.Printer (pretty)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.IO       as Text.IO
import qualified Dhall.Map          as Map
import qualified Dhall.Core         as Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Toml.Type.TOML     as T
import qualified Toml.Type.Value    as TVal


-- TODO: populate with actual errors
data CompileError
    = Unimplemented String
    | Unsupported (Expr Void Void)
    | NotARecord (Expr Void Void)


instance Show CompileError where
    show (Unimplemented s) = "unimplemented: " ++ s
    show (Unsupported e) = "unsupported: " ++ show e
    show (NotARecord e) = "The root object converted to TOML must be a record, got " ++ show e

instance Exception CompileError

dhallToToml :: Expr s Void -> Either CompileError TOML
dhallToToml e0 = do
    let norm = Core.alphaNormalize $ Core.normalize e0
    _ <- assertRecordLit norm
    toToml (mempty :: TOML) [] norm
    where
        assertRecordLit (Core.RecordLit r) = Right r
        assertRecordLit e                  = Left $ NotARecord e

toToml :: TOML -> [Piece] -> Expr Void Void -> Either CompileError TOML
toToml toml key expr  = case expr of
    Core.BoolLit a    -> insertPrim (TVal.Bool a)
    Core.NaturalLit a -> insertPrim (TVal.Integer $ toInteger a)
    Core.DoubleLit (DhallDouble a) -> insertPrim (TVal.Double a)
    Core.TextLit (Core.Chunks [] a) -> insertPrim (TVal.Text a)
    -- TODO: probe the element type, if record then table list else inline list
    -- Core.ListLit _ a -> Left $ Unimplemented
    Core.RecordLit r ->
        let
            f toml' (key', val) = toToml toml' [Piece key'] (Core.recordFieldValue val)
        in
            if null key -- at the top level, we can't have a table
            then foldM f toml (Map.toList r)
            else do
                let (flat, nested) = Map.partition (isFlat . Core.recordFieldValue) r
                -- the order here is important, at least for testing, because
                -- the PrefixMap inside TOML is dependent on insert order
                flatTable   <- foldM f mempty    (Map.toList flat)
                nestedTable <- foldM f flatTable (Map.toList nested)
                return $ T.insertTable (Key $ NonEmpty.fromList key) nestedTable toml
    _ -> Left $ Unsupported expr
    where
        insertPrim val = return $ T.insertKeyVal (Key $ NonEmpty.fromList key) val toml
        isFlat v = case v of
            Core.BoolLit _ -> True
            Core.NaturalLit _ -> True
            Core.DoubleLit _ -> True
            Core.TextLit _ -> True
            _ -> False

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
        Left err -> fail $ show err
        Right toml -> return toml
    putStrLn $ unpack $ pretty toml

tomlToDhallMain :: IO ()
tomlToDhallMain = putStrLn "not implemented"

