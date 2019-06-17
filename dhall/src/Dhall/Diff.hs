{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-| This module provides functionality for concisely displaying the difference
    between two expressions

    For example, this is used in type errors to explain why the actual type does
    not match the expected type
-}

module Dhall.Diff (
    -- * Diff
      Diff (..)
    , diffExpression
    , diffNormalized
    , Dhall.Diff.diff
    ) where

import Data.Foldable (fold, toList)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Any(..))
import Data.Semigroup
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Dhall.Core (Binding(..), Chunks (..), Const(..), Expr(..), Var(..))
import Dhall.Binary (ToTerm)
import Dhall.Map (Map)
import Dhall.Set (Set)
import Dhall.Pretty.Internal (Ann)
import Numeric.Natural (Natural)

import qualified Data.Algorithm.Diff        as Algo.Diff
import qualified Data.List.NonEmpty
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc  as Pretty
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Set
import qualified Dhall.Pretty.Internal      as Internal

data Diff =
    Diff
        { same :: Bool
        , doc  :: Doc Ann
        }

instance Data.Semigroup.Semigroup Diff where
    Diff sameL docL <> Diff sameR docR = Diff (sameL && sameR) (docL <> docR)

instance Monoid (Diff) where
    mempty = Diff {..}
      where
        same = True

        doc = mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance IsString (Diff) where
    fromString string = Diff {..}
      where
        same = True

        doc = fromString string

ignore :: Diff
ignore = "…"

align :: Diff -> Diff
align (Diff {doc = docOld, ..}) = Diff {doc = Pretty.align docOld, .. }

hardline :: Diff
hardline = token Pretty.hardline

minus :: Diff -> Diff
minus l = ("- " <> l) { same = False }

plus :: Diff -> Diff
plus r = ("+ " <> r) { same = False }

difference :: Diff -> Diff -> Diff
difference l r = align (minus l <> hardline <> plus r)

token :: Doc Ann -> Diff
token doc = Diff {..}
  where
    same = True

format :: Diff -> Diff -> Diff
format suffix doc = doc <> (if same doc then suffix else hardline)

builtin :: Doc Ann -> Diff
builtin doc = token (Internal.builtin doc)

keyword :: Doc Ann -> Diff
keyword doc = token (Internal.keyword doc)

operator :: Doc Ann -> Diff
operator doc = token (Internal.operator doc)

colon :: Diff
colon = token Internal.colon

comma :: Diff
comma = token Internal.comma

dot :: Diff
dot = token Internal.dot

equals :: Diff
equals = token Internal.equals

forall :: Diff
forall = token (Internal.forall Internal.Unicode)

lambda :: Diff
lambda = token (Internal.lambda Internal.Unicode)

langle :: Diff
langle = token Internal.langle

lbrace :: Diff
lbrace = token Internal.lbrace

lbracket :: Diff
lbracket = token Internal.lbracket

lparen :: Diff
lparen = token Internal.lparen

pipe :: Diff
pipe = token Internal.pipe

rangle :: Diff
rangle = token Internal.rangle

rarrow :: Diff
rarrow = token (Internal.rarrow Internal.Unicode)

rbrace :: Diff
rbrace = token Internal.rbrace

rbracket :: Diff
rbracket = token Internal.rbracket

rparen :: Diff
rparen = token Internal.rparen

-- | Render the difference between the normal form of two expressions
diffNormalized :: (Eq a, Pretty a, ToTerm a) => Expr s a -> Expr s a -> Doc Ann
diffNormalized l0 r0 = Dhall.Diff.diff l1 r1
  where
    l1 = Dhall.Core.alphaNormalize (Dhall.Core.normalize l0)
    r1 = Dhall.Core.alphaNormalize (Dhall.Core.normalize r0)

-- | Render the difference between two expressions
diff :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Doc Ann
diff l0 r0 = doc
  where
    Diff {..} = diffExpression l0 r0

diffPrimitive :: Eq a => (a -> Diff) -> a -> a -> Diff
diffPrimitive f l r
    | l == r    = ignore
    | otherwise = difference (f l) (f r)

diffLabel :: Text -> Text -> Diff
diffLabel = diffPrimitive (token . Internal.prettyLabel)

diffLabels :: Set Text -> Set Text -> Diff
diffLabels ksL ksR =
    braced (diffFieldNames <> (if anyEqual then [ ignore ] else []))
  where
    extraL = Dhall.Set.difference ksL ksR
    extraR = Dhall.Set.difference ksR ksL

    diffFieldNames = foldMap (adapt minus) extraL <> foldMap (adapt plus) extraR
      where
        adapt sign key = [ sign (token (Internal.prettyLabel key)) ]

    anyEqual = not (Data.Set.null (Data.Set.intersection
                                   (Dhall.Set.toSet ksL)
                                   (Dhall.Set.toSet ksR)))

diffNatural :: Natural -> Natural -> Diff
diffNatural = diffPrimitive (token . Internal.prettyNatural)

diffDouble :: Double -> Double -> Diff
diffDouble = diffPrimitive (token . Internal.prettyDouble)

diffConst :: Const -> Const -> Diff
diffConst = diffPrimitive (token . Internal.prettyConst)

diffBool :: Bool -> Bool -> Diff
diffBool = diffPrimitive bool
  where
    bool True  = builtin "True"
    bool False = builtin "False"

diffInteger :: Integer -> Integer -> Diff
diffInteger = diffPrimitive (token . Internal.prettyNumber)

diffVar :: Var -> Var -> Diff
diffVar (V xL nL) (V xR nR) = format mempty label <> "@" <> natural
  where
    label = diffLabel xL xR

    natural = diffInteger nL nR

diffPretty :: (Eq a, Pretty a) => a -> a -> Diff
diffPretty = diffPrimitive (token . Pretty.pretty)

diffMaybe :: Diff -> (a -> a -> Diff) -> (Maybe a -> Maybe a -> Diff)
diffMaybe _ _ Nothing Nothing =
    mempty
diffMaybe prefix _ Nothing (Just _) =
    difference mempty (prefix <> ignore)
diffMaybe prefix _ (Just _) Nothing =
    difference (prefix <> ignore) mempty
diffMaybe prefix f (Just l) (Just r) =
    prefix <> f l r

enclosed
    :: Diff
    -> Diff
    -> Diff
    -> [Diff]
    -> Diff
enclosed l _ r []   = l <> r
enclosed l m r docs = align (fold (zipWith (<>) prefixes docs) <> suffix)
  where
    prefixes = l : repeat (hardline <> m)

    suffix = hardline <> r

enclosed'
    :: Diff
    -> Diff
    -> NonEmpty (Diff)
    -> Diff
enclosed' l m docs =
    align (fold (Data.List.NonEmpty.zipWith (<>) prefixes docs))
  where
    prefixes = l :| repeat (hardline <> m)

diffKeyVals
    :: (Eq a, Pretty a)
    => Diff
    -> Map Text (Expr s a)
    -> Map Text (Expr s a)
    -> [Diff]
diffKeyVals assign = diffKeysWith assign diffExpression

diffKeysWith
    :: Diff
    -> (a -> a -> Diff)
    -> Map Text a
    -> Map Text a
    -> [Diff]
diffKeysWith assign diffVals kvsL kvsR =
    diffFieldNames <> diffFieldValues <> (if anyEqual then [ ignore ] else [])
  where
    ksL = Data.Set.fromList (Dhall.Map.keys kvsL)
    ksR = Data.Set.fromList (Dhall.Map.keys kvsR)

    extraL = Data.Set.difference ksL ksR
    extraR = Data.Set.difference ksR ksL

    diffFieldNames = foldMap (adapt minus) extraL <> foldMap (adapt plus) extraR
      where
        adapt sign key =
            [   sign (token (Internal.prettyLabel key))
            <>  " "
            <>  assign
            <>  " "
            <>  ignore
            ]

    shared = Dhall.Map.intersectionWith diffVals kvsL kvsR

    diffFieldValues =
        filter (not . same) (Dhall.Map.foldMapWithKey adapt shared)
      where
        adapt key doc =
            [   (if ksL == ksR then mempty else "  ")
            <>  token (Internal.prettyLabel key)
            <>  " "
            <>  assign
            <>  " "
            <>  doc
            ]

    anyEqual = getAny (foldMap (Any . same) shared)

braced :: [Diff] -> Diff
braced = enclosed (lbrace <> " ") (comma <> " ") rbrace

angled :: [Diff] -> Diff
angled = enclosed (langle <> " ") (pipe <> " ") rangle

bracketed :: [Diff] -> Diff
bracketed = enclosed (lbracket <> " ") (comma <> " ") rbracket

diffText :: Text -> Text -> Diff
diffText l r
  | null parts         = "\"\""
  | allDifferent parts = difference textSkeleton textSkeleton
  | allSame parts      = textSkeleton
  | otherwise          = "\"" <> foldMap prettyPart parts <> "\""
  where
    allDifferent = not . any isBoth
    allSame      = all isBoth

    -- TODO: check for color support from the TTY
    colorDiff colorCode chars =
            "\ESC["
        <>  colorCode
        <>  "m"
        <>  fromString chars
        <>  "\ESC[0m"

    prettyPart part =
      case part of
        -- Only present in left
        Algo.Diff.First  chars ->
            -- Red background
            (colorDiff "41" chars) { same = False }

        -- Only present in right
        Algo.Diff.Second chars ->
            -- Green background
            (colorDiff "42" chars) { same = False }

        -- Present in both
        Algo.Diff.Both _ chars ->
            -- Dim foreground
            colorDiff "2" chars

    parts = Algo.Diff.getGroupedDiff (Data.Text.unpack l) (Data.Text.unpack r)

diffChunks
    :: (Eq a, Pretty a)
    => Chunks s a -> Chunks s a -> Diff
diffChunks cL cR
  | null chunks             = "\"\""
  | [c] <- chunks           = c
  | otherwise               = align (enclosed "   " "++ " "" chunks)
  where
    toEitherList (Chunks te t) =
        concatMap (\(a, b) -> [Left a, Right b]) te ++ [Left t]

    diffTextSkeleton = difference textSkeleton textSkeleton

    chunks = zipWith chunkDiff (toEitherList cL) (toEitherList cR) 

    chunkDiff a b =
      case (a, b) of
        (Left  x, Left y ) -> diffText x y
        (Right x, Right y) -> diffExpression x y
        _                  -> diffTextSkeleton

diffList
    :: (Eq a, Pretty a)
    => Seq (Expr s a) -> Seq (Expr s a) -> Diff
diffList l r
  | allDifferent parts = difference listSkeleton listSkeleton
  | otherwise          = bracketed (foldMap diffPart parts)
  where
    allDifferent = any (not . isBoth)

    -- Sections of the list that are only in left, only in right, or in both
    parts =
        Algo.Diff.getGroupedDiffBy ((same .) . diffExpression) (toList l) (toList r)

    -- Render each element of a list using an extra rendering function f
    prettyElems f = map (f . token . Internal.prettyExpr)

    diffPart part =
      case part of
        -- Only present in left
        Algo.Diff.First  elements ->
            prettyElems minus elements

        -- Only present in right
        Algo.Diff.Second elements ->
            prettyElems plus  elements

        -- Present in both
        Algo.Diff.Both _ _        ->
            pure ignore

-- Helper function to check when a diff part is present on both sides
isBoth :: Algo.Diff.Diff a -> Bool
isBoth p
  | Algo.Diff.Both _ _ <- p = True
  | otherwise               = False

diffRecord
    :: (Eq a, Pretty a)
    => Map Text (Expr s a) -> Map Text (Expr s a) -> Diff
diffRecord kvsL kvsR = braced (diffKeyVals colon kvsL kvsR)

diffRecordLit
    :: (Eq a, Pretty a)
    => Map Text (Expr s a) -> Map Text (Expr s a) -> Diff
diffRecordLit kvsL kvsR = braced (diffKeyVals equals kvsL kvsR)

diffUnion
    :: (Eq a, Pretty a)
    => Map Text (Maybe (Expr s a)) -> Map Text (Maybe (Expr s a)) -> Diff
diffUnion kvsL kvsR = angled (diffKeysWith colon diffVals kvsL kvsR)
  where
    diffVals = diffMaybe (colon <> " ") diffExpression

diffUnionLit
    :: (Eq a, Pretty a)
    => Text
    -> Text
    -> Expr s a
    -> Expr s a
    -> Map Text (Maybe (Expr s a))
    -> Map Text (Maybe (Expr s a))
    -> Diff
diffUnionLit kL kR vL vR kvsL kvsR =
        langle
    <>  " "
    <>  format " " (diffLabel kL kR)
    <>  equals
    <>  " "
    <>  format " " (diffExpression vL vR)
    <>  halfAngled (diffKeysWith colon diffVals kvsL kvsR)
  where
    diffVals = diffMaybe (colon <> " ") diffExpression

    halfAngled = enclosed (pipe <> " ") (pipe <> " ") rangle

listSkeleton :: Diff
listSkeleton =
        lbracket
    <>  " "
    <>  ignore
    <>  " "
    <>  rbracket

textSkeleton :: Diff
textSkeleton =
        "\""
    <>  ignore
    <>  "\""

skeleton :: Pretty a => Expr s a -> Diff
skeleton (Lam {}) =
        lambda
    <>  lparen
    <>  ignore
    <>  " "
    <>  colon
    <>  " "
    <>  ignore
    <>  rparen
    <>  " "
    <>  rarrow
    <>  " "
    <>  ignore
skeleton (Pi {}) =
        ignore
    <>  " "
    <>  rarrow
    <>  " "
    <>  ignore
skeleton (App Optional _) =
        "Optional "
    <>  ignore
skeleton (App None _) =
        "None "
    <>  ignore
skeleton (Some _) =
        "Some "
    <>  ignore
skeleton (App List _) =
        "List "
    <>  ignore
skeleton (App {}) =
        ignore
    <>  " "
    <>  ignore
skeleton (Let {}) =
        keyword "let"
    <>  " "
    <>  ignore
    <>  " "
    <>  equals
    <>  " "
    <>  ignore
    <>  " "
    <>  keyword "in"
    <>  " "
    <>  ignore
skeleton (Annot {}) =
        ignore
    <>  " "
    <>  colon
    <>  " "
    <>  ignore
skeleton (BoolAnd {}) =
        ignore
    <>  " "
    <>  operator "&&"
    <>  " "
    <>  ignore
skeleton (BoolOr {}) =
        ignore
    <>  " "
    <>  operator "||"
    <>  " "
    <>  ignore
skeleton (BoolEQ {}) =
        ignore
    <>  " "
    <>  operator "=="
    <>  " "
    <>  ignore
skeleton (BoolNE {}) =
        ignore
    <>  " "
    <>  operator "!="
    <>  " "
    <>  ignore
skeleton (BoolIf {}) =
        keyword "if"
    <>  " "
    <>  ignore
    <>  " "
    <>  keyword "then"
    <>  " "
    <>  ignore
    <>  " "
    <>  keyword "else"
    <>  " "
    <>  ignore
skeleton (NaturalPlus {}) =
        ignore
    <>  " "
    <>  operator "+"
    <>  " "
    <>  ignore
skeleton (NaturalTimes {}) =
        ignore
    <>  " "
    <>  operator "*"
    <>  " "
    <>  ignore
skeleton (TextLit {}) =
        textSkeleton
skeleton (TextAppend {}) =
        ignore
    <>  " "
    <>  operator "++"
    <>  " "
    <>  ignore
skeleton (ListLit {}) =
        listSkeleton
    <>  " "
    <>  colon
    <>  " "
    <>  builtin "List"
    <>  " "
    <>  ignore
skeleton (ListAppend {}) =
        ignore
    <>  " "
    <>  operator "#"
    <>  " "
    <>  ignore
skeleton (Record {}) =
        lbrace
    <>  " "
    <>  ignore
    <>  " "
    <>  colon
    <>  " "
    <>  ignore
    <>  " "
    <>  rbrace
skeleton (RecordLit {}) =
        lbrace
    <>  " "
    <>  ignore
    <>  " "
    <>  equals
    <>  " "
    <>  ignore
    <>  " "
    <>  rbrace
skeleton (Union {}) =
        langle
    <>  " "
    <>  ignore
    <>  " "
    <>  colon
    <>  " "
    <>  ignore
    <>  " "
    <>  rangle
skeleton (UnionLit {}) =
        langle
    <>  " "
    <>  ignore
    <>  " "
    <>  equals
    <>  " "
    <>  ignore
    <>  " "
    <>  rangle
skeleton (Combine {}) =
        ignore
    <>  " "
    <>  operator "∧"
    <>  " "
    <>  ignore
skeleton (CombineTypes {}) =
        ignore
    <>  " "
    <>  operator "⩓"
    <>  " "
    <>  ignore
skeleton (Prefer {}) =
        ignore
    <>  " "
    <>  operator "⫽"
    <>  " "
    <>  ignore
skeleton (Merge {}) =
        keyword "merge"
    <>  " "
    <>  ignore
    <>  " "
    <>  ignore
skeleton (Field {}) =
        ignore
    <>  dot
    <>  ignore
skeleton (Project {}) =
        ignore
    <>  dot
    <>  lbrace
    <>  " "
    <>  ignore
    <>  " "
    <>  rbrace
skeleton x = token (Pretty.pretty x)

mismatch :: Pretty a => Expr s a -> Expr s a -> Diff
mismatch l r = difference (skeleton l) (skeleton r)

diffExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffExpression l@(Lam {}) r@(Lam {}) =
    enclosed' "  " (rarrow <> " ") (docs l r)
  where
    docs (Lam aL bL cL) (Lam aR bR cR) =
        Data.List.NonEmpty.cons (align doc) (docs cL cR)
      where
        doc =   lambda
            <>  lparen
            <>  format " " (diffLabel aL aR)
            <>  colon
            <>  " "
            <>  format mempty (diffExpression bL bR)
            <>  rparen

    docs aL aR =
        pure (diffExpression aL aR)
diffExpression l@(Lam {}) r =
    mismatch l r
diffExpression l r@(Lam {}) =
    mismatch l r
diffExpression l@(BoolIf {}) r@(BoolIf {}) =
    enclosed' "      " (keyword "else" <> "  ") (docs l r)
  where
    docs (BoolIf aL bL cL) (BoolIf aR bR cR) =
        Data.List.NonEmpty.cons (align doc) (docs cL cR)
      where
        doc =   keyword "if"
            <>  " "
            <>  format " " (diffExpression aL aR)
            <>  keyword "then"
            <>  " "
            <>  diffExpression bL bR
    docs aL aR =
        pure (diffExpression aL aR)
diffExpression l@(BoolIf {}) r =
    mismatch l r
diffExpression l r@(BoolIf {}) =
    mismatch l r
diffExpression (Let asL bL ) (Let asR bR) =
    enclosed' "" (keyword "in" <> "  ")
        (Data.List.NonEmpty.zipWith docA asL asR <> pure docB)
  where
    docA (Binding cL dL eL) (Binding cR dR eR) = align doc
      where
        doc =   keyword "let"
            <>  " "
            <>  format " " (diffLabel cL cR)
            <>  format " " (diffMaybe (colon <> " ") diffExpression dL dR)
            <>  equals
            <>  " "
            <>  diffExpression eL eR

    docB = diffExpression bL bR
diffExpression l@(Let {}) r =
    mismatch l r
diffExpression l r@(Let {}) =
    mismatch l r
diffExpression l@(Pi {}) r@(Pi {}) =
    enclosed' "  " (rarrow <> " ") (docs l r)
  where
    docs (Pi aL bL cL) (Pi aR bR cR) =
        Data.List.NonEmpty.cons (align doc) (docs cL cR)
      where
        doc | same docA && same docB = ignore
            | otherwise =
                forall
            <>  lparen
            <>  format " " docA
            <>  colon
            <>  " "
            <>  format mempty docB
            <>  rparen
          where
            docA = diffLabel aL aR

            docB = diffExpression bL bR

    docs aL aR = pure (diffExpression aL aR)
diffExpression l@(Pi {}) r =
    mismatch l r
diffExpression l r@(Pi {}) =
    mismatch l r
diffExpression l r =
    diffAnnotatedExpression l r

diffAnnotatedExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffAnnotatedExpression (Merge aL bL cL) (Merge aR bR cR) = align doc
  where
    doc =   keyword "merge"
        <>  " "
        <>  format " " (diffImportExpression aL aR)
        <>  format " " (diffImportExpression bL bR)
        <>  diffMaybe (colon <> " ") diffApplicationExpression cL cR
diffAnnotatedExpression l@(Merge {}) r =
    mismatch l r
diffAnnotatedExpression l r@(Merge {}) =
    mismatch l r
diffAnnotatedExpression (ListLit aL@(Just _) bL) (ListLit aR bR) = align doc
  where
    doc =   format " " (diffList bL bR)
        <>  format " " (diffMaybe (colon <> " ") (diffApplicationExpression `on` App List) aL aR)
diffAnnotatedExpression (ListLit aL bL) (ListLit aR@(Just _) bR) = align doc
  where
    doc =   format " " (diffList bL bR)
        <>  format " " (diffMaybe (colon <> " ") (diffApplicationExpression `on` App List) aL aR)
diffAnnotatedExpression l@(Annot {}) r@(Annot {}) =
    enclosed' "  " (colon <> " ") (docs l r)
  where
    docs (Annot aL bL) (Annot aR bR) =
        Data.List.NonEmpty.cons (align doc) (docs bL bR)
      where
        doc = diffOperatorExpression aL aR
    docs aL aR =
        diffExpression aL aR :| []
diffAnnotatedExpression l@(Annot {}) r =
    mismatch l r
diffAnnotatedExpression l r@(Annot {}) =
    mismatch l r
diffAnnotatedExpression l r =
    diffOperatorExpression l r

diffOperatorExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffOperatorExpression = diffOrExpression

diffOrExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffOrExpression l@(BoolOr {}) r@(BoolOr {}) =
    enclosed' "    " (operator "||" <> "  ") (docs l r)
  where
    docs (BoolOr aL bL) (BoolOr aR bR) =
        Data.List.NonEmpty.cons (diffTextAppendExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffTextAppendExpression aL aR)
diffOrExpression l@(BoolOr {}) r =
    mismatch l r
diffOrExpression l r@(BoolOr {}) =
    mismatch l r
diffOrExpression l r =
    diffPlusExpression l r

diffPlusExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffPlusExpression l@(NaturalPlus {}) r@(NaturalPlus {}) =
    enclosed' "  " (operator "+" <> " ") (docs l r)
  where
    docs (NaturalPlus aL bL) (NaturalPlus aR bR) =
        Data.List.NonEmpty.cons (diffListAppendExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffListAppendExpression aL aR)
diffPlusExpression l@(NaturalPlus {}) r =
    mismatch l r
diffPlusExpression l r@(NaturalPlus {}) =
    mismatch l r
diffPlusExpression l r =
    diffTextAppendExpression l r

diffTextAppendExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffTextAppendExpression l@(TextAppend {}) r@(TextAppend {}) =
    enclosed' "    " (operator "++" <> "  ") (docs l r)
  where
    docs (TextAppend aL bL) (TextAppend aR bR) =
        Data.List.NonEmpty.cons (diffPlusExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffPlusExpression aL aR)
diffTextAppendExpression l@(TextAppend {}) r =
    mismatch l r
diffTextAppendExpression l r@(TextAppend {}) =
    mismatch l r
diffTextAppendExpression l r =
    diffListAppendExpression l r

diffListAppendExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffListAppendExpression l@(ListAppend {}) r@(ListAppend {}) =
    enclosed' "  " (operator "#" <> " ") (docs l r)
  where
    docs (ListAppend aL bL) (ListAppend aR bR) =
        Data.List.NonEmpty.cons (diffAndExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffAndExpression aL aR)
diffListAppendExpression l@(ListAppend {}) r =
    mismatch l r
diffListAppendExpression l r@(ListAppend {}) =
    mismatch l r
diffListAppendExpression l r =
    diffAndExpression l r

diffAndExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffAndExpression l@(BoolAnd {}) r@(BoolAnd {}) =
    enclosed' "    " (operator "&&" <> "  ") (docs l r)
  where
    docs (BoolAnd aL bL) (BoolAnd aR bR) =
        Data.List.NonEmpty.cons (diffCombineExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffCombineExpression aL aR)
diffAndExpression l@(BoolAnd {}) r =
    mismatch l r
diffAndExpression l r@(BoolAnd {}) =
    mismatch l r
diffAndExpression l r =
    diffCombineExpression l r

diffCombineExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffCombineExpression l@(Combine {}) r@(Combine {}) =
    enclosed' "  " (operator "∧" <> " ") (docs l r)
  where
    docs (Combine aL bL) (Combine aR bR) =
        Data.List.NonEmpty.cons (diffPreferExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffPreferExpression aL aR)
diffCombineExpression l@(Combine {}) r =
    mismatch l r
diffCombineExpression l r@(Combine {}) =
    mismatch l r
diffCombineExpression l r =
    diffPreferExpression l r

diffPreferExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffPreferExpression l@(Prefer {}) r@(Prefer {}) =
    enclosed' "  " (operator "⫽" <> " ") (docs l r)
  where
    docs (Prefer aL bL) (Prefer aR bR) =
        Data.List.NonEmpty.cons (diffCombineTypesExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffCombineTypesExpression aL aR)
diffPreferExpression l@(Prefer {}) r =
    mismatch l r
diffPreferExpression l r@(Prefer {}) =
    mismatch l r
diffPreferExpression l r =
    diffCombineTypesExpression l r

diffCombineTypesExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffCombineTypesExpression l@(CombineTypes {}) r@(CombineTypes {}) =
    enclosed' "  " (operator "*" <> " ") (docs l r)
  where
    docs (CombineTypes aL bL) (CombineTypes aR bR) =
        Data.List.NonEmpty.cons (diffTimesExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffTimesExpression aL aR)
diffCombineTypesExpression l@(CombineTypes {}) r =
    mismatch l r
diffCombineTypesExpression l r@(CombineTypes {}) =
    mismatch l r
diffCombineTypesExpression l r =
    diffTimesExpression l r

diffTimesExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffTimesExpression l@(NaturalTimes {}) r@(NaturalTimes {}) =
    enclosed' "  " (operator "*" <> " ") (docs l r)
  where
    docs (NaturalTimes aL bL) (NaturalTimes aR bR) =
        Data.List.NonEmpty.cons (diffEqualExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffEqualExpression aL aR)
diffTimesExpression l@(NaturalTimes {}) r =
    mismatch l r
diffTimesExpression l r@(NaturalTimes {}) =
    mismatch l r
diffTimesExpression l r =
    diffEqualExpression l r

diffEqualExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffEqualExpression l@(BoolEQ {}) r@(BoolEQ {}) =
    enclosed' "    " (operator "==" <> "  ") (docs l r)
  where
    docs (BoolEQ aL bL) (BoolEQ aR bR) =
        Data.List.NonEmpty.cons (diffNotEqualExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffNotEqualExpression aL aR)
diffEqualExpression l@(BoolEQ {}) r =
    mismatch l r
diffEqualExpression l r@(BoolEQ {}) =
    mismatch l r
diffEqualExpression l r =
    diffNotEqualExpression l r

diffNotEqualExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffNotEqualExpression l@(BoolNE {}) r@(BoolNE {}) =
    enclosed' "    " (operator "!=" <> "  ") (docs l r)
  where
    docs (BoolNE aL bL) (BoolNE aR bR) =
        Data.List.NonEmpty.cons (diffApplicationExpression aL aR) (docs bL bR)
    docs aL aR =
        pure (diffApplicationExpression aL aR)
diffNotEqualExpression l@(BoolNE {}) r =
    mismatch l r
diffNotEqualExpression l r@(BoolNE {}) =
    mismatch l r
diffNotEqualExpression l r =
    diffApplicationExpression l r

diffApplicationExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffApplicationExpression l@(App {}) r@(App {}) =
    enclosed' mempty mempty (Data.List.NonEmpty.reverse (docs l r))
  where
    docs (App aL bL) (App aR bR) =
        Data.List.NonEmpty.cons (diffImportExpression bL bR) (docs aL aR)
    docs (Some aL) (Some aR) =
        diffImportExpression aL aR :| [ builtin "Some" ]
    docs aL aR@(Some {}) =
        pure (mismatch aL aR)
    docs aL@(Some {}) aR =
        pure (mismatch aL aR)
    docs aL aR =
        pure (diffImportExpression aL aR)
diffApplicationExpression l@(App {}) r =
    mismatch l r
diffApplicationExpression l r@(App {}) =
    mismatch l r
diffApplicationExpression (Some l) (Some r) =
    enclosed' mempty mempty (builtin "Some" :| [ diffImportExpression l r ])
diffApplicationExpression l@(Some {}) r =
    mismatch l r
diffApplicationExpression l r@(Some {}) =
    mismatch l r
diffApplicationExpression l r =
    diffImportExpression l r

diffImportExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffImportExpression (Embed l) (Embed r) =
    diffPretty l r
diffImportExpression l@(Embed {}) r =
    mismatch l r
diffImportExpression l r@(Embed {}) =
    mismatch l r
diffImportExpression l r =
    diffSelectorExpression l r

diffSelectorExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffSelectorExpression l@(Field {}) r@(Field {}) =
    enclosed' "  " (dot <> " ") (Data.List.NonEmpty.reverse (docs l r))
  where
    docs (Field aL bL) (Field aR bR) =
        Data.List.NonEmpty.cons (diffLabel bL bR) (docs aL aR)
    docs (Project aL (Left bL)) (Project aR (Left bR)) =
        Data.List.NonEmpty.cons (diffLabels bL bR) (docs aL aR)
    docs (Project aL (Right bL)) (Project aR (Right bR)) =
        Data.List.NonEmpty.cons (diffExpression bL bR) (docs aL aR)
    docs aL aR =
        pure (diffPrimitiveExpression aL aR)
diffSelectorExpression l@(Field {}) r =
    mismatch l r
diffSelectorExpression l r@(Field {}) =
    mismatch l r
diffSelectorExpression l@(Project {}) r@(Project {}) =
    enclosed' "  " (dot <> " ") (Data.List.NonEmpty.reverse (docs l r))
  where
    docs (Field aL bL) (Field aR bR) =
        Data.List.NonEmpty.cons (diffLabel bL bR) (docs aL aR)
    docs (Project aL (Left bL)) (Project aR (Left bR)) =
        Data.List.NonEmpty.cons (diffLabels bL bR) (docs aL aR)
    docs (Project aL (Right bL)) (Project aR (Right bR)) =
        Data.List.NonEmpty.cons (diffExpression bL bR) (docs aL aR)
    docs aL aR =
        pure (diffPrimitiveExpression aL aR)
diffSelectorExpression l@(Project {}) r =
    mismatch l r
diffSelectorExpression l r@(Project {}) =
    mismatch l r
diffSelectorExpression l r =
    diffPrimitiveExpression l r

diffPrimitiveExpression :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Diff
diffPrimitiveExpression (Var aL) (Var aR) =
    diffVar aL aR
diffPrimitiveExpression l@(Var {}) r =
    mismatch l r
diffPrimitiveExpression l r@(Var {}) =
    mismatch l r
diffPrimitiveExpression (Const aL) (Const aR) =
    diffConst aL aR
diffPrimitiveExpression l@(Const {}) r =
    mismatch l r
diffPrimitiveExpression l r@(Const {}) =
    mismatch l r
diffPrimitiveExpression Bool Bool =
    "…"
diffPrimitiveExpression l@Bool r =
    mismatch l r
diffPrimitiveExpression l r@Bool =
    mismatch l r
diffPrimitiveExpression Natural Natural =
    "…"
diffPrimitiveExpression l@Natural r =
    mismatch l r
diffPrimitiveExpression l r@Natural =
    mismatch l r
diffPrimitiveExpression NaturalFold NaturalFold =
    "…"
diffPrimitiveExpression l@NaturalFold r =
    mismatch l r
diffPrimitiveExpression l r@NaturalFold =
    mismatch l r
diffPrimitiveExpression NaturalBuild NaturalBuild =
    "…"
diffPrimitiveExpression l@NaturalBuild r =
    mismatch l r
diffPrimitiveExpression l r@NaturalBuild =
    mismatch l r
diffPrimitiveExpression NaturalIsZero NaturalIsZero =
    "…"
diffPrimitiveExpression l@NaturalIsZero r =
    mismatch l r
diffPrimitiveExpression l r@NaturalIsZero =
    mismatch l r
diffPrimitiveExpression NaturalEven NaturalEven =
    "…"
diffPrimitiveExpression l@NaturalEven r =
    mismatch l r
diffPrimitiveExpression l r@NaturalEven =
    mismatch l r
diffPrimitiveExpression NaturalOdd NaturalOdd =
    "…"
diffPrimitiveExpression l@NaturalOdd r =
    mismatch l r
diffPrimitiveExpression l r@NaturalOdd =
    mismatch l r
diffPrimitiveExpression NaturalToInteger NaturalToInteger =
    "…"
diffPrimitiveExpression l@NaturalToInteger r =
    mismatch l r
diffPrimitiveExpression l r@NaturalToInteger =
    mismatch l r
diffPrimitiveExpression NaturalShow NaturalShow =
    "…"
diffPrimitiveExpression l@NaturalShow r =
    mismatch l r
diffPrimitiveExpression l r@NaturalShow =
    mismatch l r
diffPrimitiveExpression Integer Integer =
    "…"
diffPrimitiveExpression l@Integer r =
    mismatch l r
diffPrimitiveExpression l r@Integer =
    mismatch l r
diffPrimitiveExpression IntegerShow IntegerShow =
    "…"
diffPrimitiveExpression l@IntegerShow r =
    mismatch l r
diffPrimitiveExpression l r@IntegerShow =
    mismatch l r
diffPrimitiveExpression IntegerToDouble IntegerToDouble =
    "…"
diffPrimitiveExpression l@IntegerToDouble r =
    mismatch l r
diffPrimitiveExpression l r@IntegerToDouble =
    mismatch l r
diffPrimitiveExpression Double Double =
    "…"
diffPrimitiveExpression l@Double r =
    mismatch l r
diffPrimitiveExpression l r@Double =
    mismatch l r
diffPrimitiveExpression DoubleShow DoubleShow =
    "…"
diffPrimitiveExpression l@DoubleShow r =
    mismatch l r
diffPrimitiveExpression l r@DoubleShow =
    mismatch l r
diffPrimitiveExpression Text Text =
    "…"
diffPrimitiveExpression l@Text r =
    mismatch l r
diffPrimitiveExpression l r@Text =
    mismatch l r
diffPrimitiveExpression TextShow TextShow =
    "…"
diffPrimitiveExpression l@TextShow r =
    mismatch l r
diffPrimitiveExpression l r@TextShow =
    mismatch l r
diffPrimitiveExpression List List =
    "…"
diffPrimitiveExpression l@List r =
    mismatch l r
diffPrimitiveExpression l r@List =
    mismatch l r
diffPrimitiveExpression (ListLit Nothing bL) (ListLit Nothing bR) = align doc
  where
    doc = format " " (diffList bL bR)
diffPrimitiveExpression ListBuild ListBuild =
    "…"
diffPrimitiveExpression l@ListBuild r =
    mismatch l r
diffPrimitiveExpression l r@ListBuild =
    mismatch l r
diffPrimitiveExpression ListFold ListFold =
    "…"
diffPrimitiveExpression l@ListFold r =
    mismatch l r
diffPrimitiveExpression l r@ListFold =
    mismatch l r
diffPrimitiveExpression ListLength ListLength =
    "…"
diffPrimitiveExpression l@ListLength r =
    mismatch l r
diffPrimitiveExpression l r@ListLength =
    mismatch l r
diffPrimitiveExpression ListHead ListHead =
    "…"
diffPrimitiveExpression l@ListHead r =
    mismatch l r
diffPrimitiveExpression l r@ListHead =
    mismatch l r
diffPrimitiveExpression ListLast ListLast =
    "…"
diffPrimitiveExpression l@ListLast r =
    mismatch l r
diffPrimitiveExpression l r@ListLast =
    mismatch l r
diffPrimitiveExpression ListIndexed ListIndexed =
    "…"
diffPrimitiveExpression l@ListIndexed r =
    mismatch l r
diffPrimitiveExpression l r@ListIndexed =
    mismatch l r
diffPrimitiveExpression ListReverse ListReverse =
    "…"
diffPrimitiveExpression l@ListReverse r =
    mismatch l r
diffPrimitiveExpression l r@ListReverse =
    mismatch l r
diffPrimitiveExpression Optional Optional =
    "…"
diffPrimitiveExpression l@Optional r =
    mismatch l r
diffPrimitiveExpression l r@Optional =
    mismatch l r
diffPrimitiveExpression None None =
    "…"
diffPrimitiveExpression l@None r =
    mismatch l r
diffPrimitiveExpression l r@None =
    mismatch l r
diffPrimitiveExpression OptionalFold OptionalFold =
    "…"
diffPrimitiveExpression l@OptionalFold r =
    mismatch l r
diffPrimitiveExpression l r@OptionalFold =
    mismatch l r
diffPrimitiveExpression OptionalBuild OptionalBuild =
    "…"
diffPrimitiveExpression l@OptionalBuild r =
    mismatch l r
diffPrimitiveExpression l r@OptionalBuild =
    mismatch l r
diffPrimitiveExpression (BoolLit aL) (BoolLit aR) =
    diffBool aL aR
diffPrimitiveExpression l@(BoolLit {}) r =
    mismatch l r
diffPrimitiveExpression l r@(BoolLit {}) =
    mismatch l r
diffPrimitiveExpression (IntegerLit aL) (IntegerLit aR) =
    diffInteger aL aR
diffPrimitiveExpression l@(IntegerLit {}) r =
    mismatch l r
diffPrimitiveExpression l r@(IntegerLit {}) =
    mismatch l r
diffPrimitiveExpression (NaturalLit aL) (NaturalLit aR) =
    diffNatural aL aR
diffPrimitiveExpression l@(NaturalLit {}) r =
    mismatch l r
diffPrimitiveExpression l r@(NaturalLit {}) =
    mismatch l r
diffPrimitiveExpression (DoubleLit aL) (DoubleLit aR) =
    diffDouble aL aR
diffPrimitiveExpression l@(DoubleLit {}) r =
    mismatch l r
diffPrimitiveExpression l r@(DoubleLit {}) =
    mismatch l r
diffPrimitiveExpression (TextLit l) (TextLit r) =
    diffChunks l r
diffPrimitiveExpression l@(TextLit {}) r =
    mismatch l r
diffPrimitiveExpression l r@(TextLit {}) =
    mismatch l r
diffPrimitiveExpression (Record aL) (Record aR) =
    diffRecord aL aR
diffPrimitiveExpression l@(Record {}) r =
    mismatch l r
diffPrimitiveExpression l r@(Record {}) =
    mismatch l r
diffPrimitiveExpression (RecordLit aL) (RecordLit aR) =
    diffRecordLit aL aR
diffPrimitiveExpression l@(RecordLit {}) r =
    mismatch l r
diffPrimitiveExpression l r@(RecordLit {}) =
    mismatch l r
diffPrimitiveExpression (Union aL) (Union aR) =
    diffUnion aL aR
diffPrimitiveExpression l@(Union {}) r =
    mismatch l r
diffPrimitiveExpression l r@(Union {}) =
    mismatch l r
diffPrimitiveExpression (UnionLit aL bL cL) (UnionLit aR bR cR) =
    diffUnionLit aL aR bL bR cL cR
diffPrimitiveExpression l@(UnionLit {}) r =
    mismatch l r
diffPrimitiveExpression l r@(UnionLit {}) =
    mismatch l r
diffPrimitiveExpression aL aR =
    if same doc
    then ignore
    else align ("( " <> doc <> hardline <> ")")
  where
    doc = diffExpression aL aR
