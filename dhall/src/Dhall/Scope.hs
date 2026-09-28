{-| Where a name is bound, and where it is used.

    This is the name resolution that @dhall-docs@ used to keep to itself, so
    the language server and the documentation generator share one walk.

    Resolution is de Bruijn-aware: @x\@n@ refers to the @n@th binding of @x@
    out from the use.  Binders covered here are @let@, lambda, @forall@,
    record fields and union constructors.
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
    , unionConstructorSpans
    ) where

import Control.Applicative ((<|>))
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
    | UnionConstructors (Set.Set NameDecl)
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

-- | Name spans of the alternatives in one union literal.
--
--   'Union' records only the names, not where they were written.  @src@ is
--   the literal, including the angle brackets.  The result is empty when
--   @src@ is not a union literal.
unionConstructorSpans :: Src -> [(Text, Src)]
unionConstructorSpans Src { srcStart, srcText } =
    case open (Cur srcStart srcText Nothing) of
        Just pairs ->
            pairs
        Nothing ->
            []
  where
    open cur0 =
        case consume "<" (skipSpace cur0) of
            Nothing ->
                Nothing
            Just cur1 ->
                Just (alternatives (skipBar (skipSpace cur1)) [])

    alternatives cur acc =
        let cur1 = skipSpace cur
        in if ends cur1
            then reverse acc
            else case readLabel cur1 of
                Nothing ->
                    reverse acc
                Just (name, nameSrc, cur2) ->
                    let cur3 = skipPayload (skipSpace cur2)
                        cur4 = skipSpace cur3
                        acc' = (name, nameSrc) : acc
                    in case consume "|" cur4 of
                        Just cur5 ->
                            alternatives cur5 acc'
                        Nothing ->
                            reverse acc'

    ends cur =
        Text.null (remaining cur) || Text.isPrefixOf ">" (remaining cur)

    skipBar cur =
        case consume "|" cur of
            Just cur' ->
                cur'
            Nothing ->
                cur

    skipPayload cur =
        case consume ":" (skipSpace cur) of
            Nothing ->
                cur
            Just cur' ->
                skipExpr stopsBeforeBarOrAngle (skipSpace cur')

    readLabel cur =
        case Text.uncons (remaining cur) of
            Just ('`', _) ->
                readQuoted cur
            Just (c, _)
                | headChar c ->
                    readSimple cur
            _ ->
                Nothing

    readQuoted cur =
        case consume "`" cur of
            Nothing ->
                Nothing
            Just cur1 ->
                let (body, rest) = Text.span quotedChar (remaining cur1)
                    raw = "`" <> body <> "`"
                    curBody = Cur (advanceText (curPos cur1) body) rest (lastChar body <|> curPrev cur1)
                in case consume "`" curBody of
                    Nothing ->
                        Nothing
                    Just cur2 ->
                        Just (body, spanOf cur raw, cur2)

    readSimple cur =
        let (body, rest) = Text.splitAt 1 (remaining cur)
            (tail_, _) = Text.span tailChar rest
            raw = body <> tail_
        in Just (raw, spanOf cur raw, advanceOver cur raw)

    spanOf cur raw =
        Src (curPos cur) (advanceText (curPos cur) raw) raw

    quotedChar c =
           '\x20' <= c && c <= '\x5F'
        || '\x61' <= c && c <= '\x7E'

    headChar c =
        ('\x41' <= c && c <= '\x5A') || ('\x61' <= c && c <= '\x7A') || c == '_'

    tailChar c =
        headChar c || ('\x30' <= c && c <= '\x39') || c == '-' || c == '/'

-- | Skip a type or expression.  @stop@ says when to stop, without consuming
--   that character, once no brackets, braces, parentheses or lists are open.
--   The second argument is the character before the one under consideration.
skipExpr :: (Char -> Maybe Char -> Bool) -> Cur -> Cur
skipExpr stop = go 0 0 0 0
  where
    go ang br pa bk cur0 =
        let cur = skipSpace cur0
            prev = curPrev cur
        in case Text.uncons (remaining cur) of
            Nothing ->
                cur
            Just (c, _)
                | ang == 0 && br == 0 && pa == 0 && bk == 0 && stop c prev ->
                    cur
                | Text.isPrefixOf "''" (remaining cur) && newlineAfterQuotes cur ->
                    go ang br pa bk (skipSingleQuote cur)
                | Text.isPrefixOf "0x\"" (remaining cur) ->
                    go ang br pa bk (skipBytes cur)
                | c == '"' ->
                    go ang br pa bk (skipDoubleQuote (bump cur))
                | c == '<' ->
                    go (ang + 1) br pa bk (bump cur)
                | c == '>' && prev == Just '-' ->
                    go ang br pa bk (bump cur)
                | c == '>' && ang > 0 ->
                    go (ang - 1) br pa bk (bump cur)
                | c == '{' ->
                    go ang (br + 1) pa bk (bump cur)
                | c == '}' && br > 0 ->
                    go ang (br - 1) pa bk (bump cur)
                | c == '(' ->
                    go ang br (pa + 1) bk (bump cur)
                | c == ')' && pa > 0 ->
                    go ang br (pa - 1) bk (bump cur)
                | c == '[' ->
                    go ang br pa (bk + 1) (bump cur)
                | c == ']' && bk > 0 ->
                    go ang br pa (bk - 1) (bump cur)
                | otherwise ->
                    go ang br pa bk (bump cur)

    newlineAfterQuotes cur =
        case Text.stripPrefix "''" (remaining cur) of
            Just rest ->
                Text.isPrefixOf "\n" rest || Text.isPrefixOf "\r\n" rest
            Nothing ->
                False

    skipBytes cur =
        case Text.breakOn "\"" (Text.drop 3 (remaining cur)) of
            (_, rest)
                | Text.null rest ->
                    advanceOver cur (remaining cur)
                | otherwise ->
                    let eaten = Text.take (Text.length (remaining cur) - Text.length rest + 1) (remaining cur)
                    in advanceOver cur eaten

    skipDoubleQuote cur =
        case Text.uncons (remaining cur) of
            Nothing ->
                cur
            Just ('\\', _) ->
                skipDoubleQuote (skipEscape (bump cur))
            Just ('$', rest)
                | Text.isPrefixOf "{" rest ->
                    let cur1 = skipExpr (\c _ -> c == '}') (advanceOver cur "${")
                    in skipDoubleQuote (bump cur1)
            Just ('"', _) ->
                bump cur
            Just _ ->
                skipDoubleQuote (bump cur)

    skipEscape cur =
        case Text.uncons (remaining cur) of
            Just ('u', rest)
                | Text.isPrefixOf "{" rest ->
                    case Text.breakOn "}" (Text.drop 1 rest) of
                        (_, closing)
                            | not (Text.null closing) ->
                                advanceOver cur (Text.take (Text.length (remaining cur) - Text.length closing + 1) (remaining cur))
                            | otherwise ->
                                advanceOver cur (remaining cur)
                | otherwise ->
                    advanceOver cur (Text.take 5 (remaining cur))
            Just _ ->
                bump cur
            Nothing ->
                cur

    skipSingleQuote cur0 =
        let cur = dropPrefix "''" cur0
            cur1 = dropNewline cur
        in body cur1
      where
        body cur =
            case () of
                _
                    | Text.isPrefixOf "'''" (remaining cur) ->
                        body (dropPrefix "'''" cur)
                    | Text.isPrefixOf "''${" (remaining cur) ->
                        let cur1 = skipExpr (\c _ -> c == '}') (advanceOver cur "''${")
                        in body (bump cur1)
                    | Text.isPrefixOf "''" (remaining cur) ->
                        dropPrefix "''" cur
                    | otherwise ->
                        case Text.uncons (remaining cur) of
                            Nothing ->
                                cur
                            Just _ ->
                                body (bump cur)

        dropNewline cur
            | Text.isPrefixOf "\r\n" (remaining cur) =
                dropPrefix "\r\n" cur
            | Text.isPrefixOf "\n" (remaining cur) =
                dropPrefix "\n" cur
            | otherwise =
                cur

data Cur = Cur
    { curPos :: SourcePos
    , remaining :: Text
    , curPrev :: Maybe Char
    }

skipSpace :: Cur -> Cur
skipSpace cur =
    case Text.uncons (remaining cur) of
        Just (c, _)
            | c == ' ' || c == '\t' || c == '\n' ->
                skipSpace (bump cur)
        _
            | Text.isPrefixOf "\r\n" (remaining cur) ->
                skipSpace (dropPrefix "\r\n" cur)
            | Text.isPrefixOf "--" (remaining cur) ->
                skipSpace (dropLineComment cur)
            | Text.isPrefixOf "{-" (remaining cur) ->
                skipSpace (skipBlock (dropPrefix "{-" cur) 1)
            | otherwise ->
                cur

dropLineComment :: Cur -> Cur
dropLineComment cur =
    let (comment, _) = Text.break (\c -> c == '\n' || c == '\r') (remaining cur)
    in advanceOver cur comment

skipBlock :: Cur -> Int -> Cur
skipBlock cur 0 =
    cur
skipBlock cur depth
    | Text.null (remaining cur) =
        cur
    | Text.isPrefixOf "-}" (remaining cur) =
        skipBlock (dropPrefix "-}" cur) (depth - 1)
    | Text.isPrefixOf "{-" (remaining cur) =
        skipBlock (dropPrefix "{-" cur) (depth + 1)
    | otherwise =
        skipBlock (bump cur) depth

consume :: Text -> Cur -> Maybe Cur
consume token cur
    | Text.isPrefixOf token (remaining cur) =
        Just (advanceOver cur token)
    | otherwise =
        Nothing

advanceOver :: Cur -> Text -> Cur
advanceOver cur eaten =
    Cur
        (advanceText (curPos cur) eaten)
        (Text.drop (Text.length eaten) (remaining cur))
        (lastChar eaten <|> curPrev cur)

lastChar :: Text -> Maybe Char
lastChar text =
    case Text.unsnoc text of
        Nothing ->
            Nothing
        Just (_, c) ->
            Just c

dropPrefix :: Text -> Cur -> Cur
dropPrefix token cur =
    advanceOver cur token

bump :: Cur -> Cur
bump cur =
    case Text.uncons (remaining cur) of
        Nothing ->
            cur
        Just (c, _) ->
            advanceOver cur (Text.singleton c)

stopsBeforeBarOrAngle :: Char -> Maybe Char -> Bool
stopsBeforeBarOrAngle c prev =
    (c == '|' || c == '>') && prev /= Just '-'

advanceText :: SourcePos -> Text -> SourcePos
advanceText =
    Text.foldl advance

advance :: SourcePos -> Char -> SourcePos
advance (SourcePos name line _) '\n' =
    SourcePos name (SourcePos.mkPos (SourcePos.unPos line + 1)) (SourcePos.mkPos 1)
advance (SourcePos name line col) '\t' =
    let w = 8
        c = SourcePos.unPos col - 1
    in SourcePos name line (SourcePos.mkPos (c + w - (c `rem` w) + 1))
advance (SourcePos name line col) _ =
    SourcePos name line (SourcePos.mkPos (SourcePos.unPos col + 1))

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
                return (labelsOf dhallType)
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

        -- Constructor names are not stored on 'Union'.  A parsed literal is
        -- wrapped in a 'Note', and that source is where the names are.
        Note src (Union alternatives) ->
            unionAlternatives (Just src) context alternatives

        Union alternatives ->
            unionAlternatives Nothing context alternatives

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

        labelsOf NoInfo =
            []
        labelsOf (RecordFields s) =
            Set.toList s
        labelsOf (UnionConstructors s) =
            Set.toList s

        unionAlternatives mSrc context alternatives = do
            let located = maybe [] unionConstructorSpans mSrc
            decls <- mapM (oneAlternative located) (Map.toList alternatives)
            return (UnionConstructors (Set.fromList (concat decls)))
          where
            oneAlternative located (name, mType) = do
                case mType of
                    Nothing ->
                        return ()
                    Just ty -> do
                        _ <- infer context ty
                        return ()
                case lookup name located of
                    Nothing ->
                        return []
                    Just nameSrc -> do
                        let nameDecl = NameDecl nameSrc name NoInfo
                        Writer.tell [ScopeFragment nameSrc (NameDeclaration nameDecl)]
                        return [nameDecl]
