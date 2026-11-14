{-| Where a name is bound, and where it is used.

    This is the name resolution that @dhall-docs@ used to keep to itself, so
    the language server and the documentation generator share one walk.

    Resolution is de Bruijn-aware: @x\@n@ refers to the @n@th binding of @x@
    out from the use.  Binders covered here are @let@, lambda, @forall@ and
    record fields.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Dhall.Scope
    ( NameDecl(..)
    , JtdInfo(..)
    , ScopeFragment(..)
    , ScopeKind(..)
    , scopeFragments
    , bindingLabel
    , makeSrcForLabel
    ) where

import Control.Monad.Trans.Writer.Strict (Writer)
import Data.Text (Text)
import Dhall.Context (Context)
import Dhall.Core
    ( Binding (..)
    , Expr (..)
    , FieldSelection (..)
    , FunctionBinding (..)
    , RecordField (..)
    , Var (..)
    )
import Dhall.Src (Src (..))
import Text.Megaparsec.Pos (SourcePos (..))

import qualified Control.Monad.Trans.Writer.Strict as Writer
import qualified Data.List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Dhall.Context as Context
import qualified Dhall.Core as Core
import qualified Dhall.Map as Map
import qualified Lens.Micro as Lens
import qualified Text.Megaparsec.Pos as SourcePos

-- | What a binder denotes, for field navigation.
data JtdInfo
    = RecordFields (Set.Set NameDecl)
    | NoInfo
    deriving (Eq, Ord, Show)

-- | A binder, unique by the source span of its name.
data NameDecl = NameDecl Src Text JtdInfo
    deriving (Eq, Ord, Show)

-- | One name site in source order.
data ScopeFragment a = ScopeFragment Src (ScopeKind a)

-- | Whether this span declares a name, uses one, or is an import.
data ScopeKind a
    = NameDeclaration NameDecl
    | NameUse NameDecl
    | ImportSite a
    deriving (Eq, Ord, Show)

-- | @x\@n@ when @n@ is not zero.  @x\@0@ is just @x@.
bindingLabel :: Text -> Int -> Text
bindingLabel name 0 = name
bindingLabel name index = name <> "@" <> Text.pack (show index)

-- | Source span of a label sitting between two recorded positions.
makeSrcForLabel
    :: SourcePos
    -> SourcePos
    -> Text
    -> Src
makeSrcForLabel srcStart srcEnd name = Src {..}
  where
    realLength = getSourceColumn srcEnd - getSourceColumn srcStart
    srcText =
        if Text.length name == realLength then name
        else "`" <> name <> "`"

getSourceColumn :: SourcePos -> Int
getSourceColumn = SourcePos.unPos . SourcePos.sourceColumn

getSourceLine :: SourcePos -> Int
getSourceLine = SourcePos.unPos . SourcePos.sourceLine

-- | Every declaration and use in @expr@, in source order.
--
--   Unused declarations are kept.  Callers that only want names that are
--   referenced can filter on 'NameUse'.
scopeFragments :: Expr Src a -> [ScopeFragment a]
scopeFragments expr =
    Data.List.sortBy sorter (Writer.execWriter (infer Context.empty expr))
  where
    sorter (ScopeFragment Src{srcStart = srcStart0} _)
           (ScopeFragment Src{srcStart = srcStart1} _) =
        (getSourceLine srcStart0, getSourceColumn srcStart0)
            `compare`
        (getSourceLine srcStart1, getSourceColumn srcStart1)

    infer :: Context NameDecl -> Expr Src a -> Writer [ScopeFragment a] JtdInfo
    infer context = \case
        Note src (Embed a) ->
            Writer.tell [ScopeFragment src (ImportSite a)] >> return NoInfo

        Let (Binding
                (Just Src { srcEnd = srcEnd0 })
                name
                (Just Src { srcStart = srcStart1 })
                annotation
                _
                value) expr' -> do
            case annotation of
                Nothing -> return ()
                Just (_, t) -> do
                    _ <- infer context t
                    return ()
            bindingJtdInfo <- infer context value
            let nameSrc = makeSrcForLabel srcEnd0 srcStart1 name
                nameDecl = NameDecl nameSrc name bindingJtdInfo
            Writer.tell [ScopeFragment nameSrc (NameDeclaration nameDecl)]
            infer (Context.insert name nameDecl context) expr'

        Note src (Var (V name index)) ->
            case Context.lookup name index context of
                Nothing -> return NoInfo
                Just nameDecl@(NameDecl _ _ t) -> do
                    Writer.tell [ScopeFragment src (NameUse nameDecl)]
                    return t

        Lam _ (FunctionBinding
                (Just Src{srcEnd = srcEnd0})
                name
                (Just Src{srcStart = srcStart1})
                _
                t) expr' -> do
            dhallType <- infer context t
            let nameSrc = makeSrcForLabel srcEnd0 srcStart1 name
                nameDecl = NameDecl nameSrc name dhallType
            Writer.tell [ScopeFragment nameSrc (NameDeclaration nameDecl)]
            infer (Context.insert name nameDecl context) expr'

        -- The binder name has no span of its own.  Uses still resolve, and
        -- the declaration span is the whole @forall@ when it is wrapped in a
        -- 'Note' (handled below).
        Pi _ name ty body -> do
            _ <- infer context ty
            infer (Context.insert name (synthetic name NoInfo) context) body

        Note src (Pi _ name ty body) -> do
            _ <- infer context ty
            let nameDecl = NameDecl src name NoInfo
            Writer.tell [ScopeFragment src (NameDeclaration nameDecl)]
            infer (Context.insert name nameDecl context) body

        Field e (FieldSelection (Just Src{srcEnd = posStart}) label (Just Src{srcStart = posEnd})) -> do
            fields <- do
                dhallType <- infer context e
                case dhallType of
                    NoInfo -> return mempty
                    RecordFields s -> return (Set.toList s)
            let src = makeSrcForLabel posStart posEnd label
                match (NameDecl _ l _) = l == label
            case filter match fields of
                x@(NameDecl _ _ t) : _ -> do
                    Writer.tell [ScopeFragment src (NameUse x)]
                    return t
                _ -> return NoInfo

        RecordLit (Map.toList -> pairs) ->
            handleRecordLike pairs

        Record (Map.toList -> pairs) ->
            handleRecordLike pairs

        Note _ e ->
            infer context e

        e -> do
            mapM_ (infer context) (Lens.toListOf Core.subExpressions e)
            return NoInfo
      where
        synthetic name info =
            NameDecl
                (Src
                    (SourcePos.initialPos "<synthetic>")
                    (SourcePos.initialPos "<synthetic>")
                    name)
                name
                info

        handleRecordLike pairs =
            RecordFields . Set.fromList . concat <$> mapM one pairs
          where
            one (key, RecordField (Just Src{srcEnd = startPos}) val (Just Src{srcStart = endPos}) _) = do
                dhallType <- infer context val
                let nameSrc = makeSrcForLabel startPos endPos key
                    nameDecl = NameDecl nameSrc key dhallType
                Writer.tell [ScopeFragment nameSrc (NameDeclaration nameDecl)]
                return [nameDecl]
            one _ = return []
