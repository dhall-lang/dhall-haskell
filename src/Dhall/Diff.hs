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
      diffNormalized
    , Dhall.Diff.diff
    ) where

import Data.Foldable (fold)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Monoid (Any(..))
import Data.Scientific (Scientific)
import Data.Semigroup
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Data.List.NonEmpty (NonEmpty(..))
import Dhall.Core (Const(..), Expr(..), Var(..))
import Dhall.Pretty.Internal (Ann)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List.NonEmpty
import qualified Data.Set
import qualified Data.Text                  as Text
import qualified Data.Text.Prettyprint.Doc  as Pretty
import qualified Dhall.Core
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
forall = token Internal.forall

lambda :: Diff
lambda = token Internal.lambda

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
rarrow = token Internal.rarrow

rbrace :: Diff
rbrace = token Internal.rbrace

rbracket :: Diff
rbracket = token Internal.rbracket

rparen :: Diff
rparen = token Internal.rparen

-- | Render the difference between the normal form of two expressions
diffNormalized :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Doc Ann
diffNormalized l0 r0 = Dhall.Diff.diff l1 r1
  where
    l1 = Dhall.Core.alphaNormalize (Dhall.Core.normalize l0)
    r1 = Dhall.Core.alphaNormalize (Dhall.Core.normalize r0)

-- | Render the difference between two expressions
diff :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Doc Ann
diff l0 r0 = doc
  where
    Diff {..} = diffExprA l0 r0

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
    extraL = Data.Set.difference ksL ksR
    extraR = Data.Set.difference ksR ksL

    diffFieldNames = foldMap (adapt minus) extraL <> foldMap (adapt plus) extraR
      where
        adapt sign key = [ sign (token (Internal.prettyLabel key)) ]

    anyEqual = not (Data.Set.null (Data.Set.intersection ksL ksR))

diffNatural :: Natural -> Natural -> Diff
diffNatural = diffPrimitive (token . Internal.prettyNatural)

diffScientific :: Scientific -> Scientific -> Diff
diffScientific = diffPrimitive (token . Internal.prettyScientific)

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
    :: Pretty a
    => Diff
    -> InsOrdHashMap Text (Expr s a)
    -> InsOrdHashMap Text (Expr s a)
    -> [Diff]
diffKeyVals assign kvsL kvsR =
    diffFieldNames <> diffFieldValues <> (if anyEqual then [ ignore ] else [])
  where
    ksL = Data.Set.fromList (HashMap.keys kvsL)
    ksR = Data.Set.fromList (HashMap.keys kvsR)

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

    shared = HashMap.intersectionWith diffExprA kvsL kvsR

    diffFieldValues =
        filter (not . same) (HashMap.foldMapWithKey adapt shared)
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

diffRecord
    :: Pretty a
    => InsOrdHashMap Text (Expr s a) -> InsOrdHashMap Text (Expr s a) -> Diff
diffRecord kvsL kvsR = braced (diffKeyVals colon kvsL kvsR)

diffRecordLit
    :: Pretty a
    => InsOrdHashMap Text (Expr s a) -> InsOrdHashMap Text (Expr s a) -> Diff
diffRecordLit kvsL kvsR = braced (diffKeyVals equals kvsL kvsR)

diffUnion
    :: Pretty a
    => InsOrdHashMap Text (Expr s a) -> InsOrdHashMap Text (Expr s a) -> Diff
diffUnion kvsL kvsR = angled (diffKeyVals colon kvsL kvsR)

diffUnionLit
    :: Pretty a
    => Text
    -> Text
    -> Expr s a
    -> Expr s a
    -> InsOrdHashMap Text (Expr s a)
    -> InsOrdHashMap Text (Expr s a)
    -> Diff
diffUnionLit kL kR vL vR kvsL kvsR =
        langle
    <>  " "
    <>  format " " (diffLabel kL kR)
    <>  equals
    <>  " "
    <>  format " " (diffExprA vL vR)
    <>  halfAngled (diffKeyVals equals kvsL kvsR)
  where
    halfAngled = enclosed (pipe <> " ") (pipe <> " ") rangle

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
        forall
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
        "\""
    <>  ignore
    <>  "\""
skeleton (TextAppend {}) =
        ignore
    <>  " "
    <>  operator "++"
    <>  " "
    <>  ignore
skeleton (ListLit {}) =
        lbracket
    <>  " "
    <>  ignore
    <>  " "
    <>  rbracket
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
skeleton (OptionalLit {}) =
        lbracket
    <>  " "
    <>  ignore
    <>  " "
    <>  rbracket
    <>  " "
    <>  colon
    <>  " "
    <>  builtin "Optional"
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
skeleton (Constructors {}) =
        keyword "constructors"
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

diffExprA :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprA l@(Annot {}) r@(Annot {}) =
    enclosed' "  " (colon <> " ") (docs l r)
  where
    docs (Annot aL bL) (Annot aR bR) =
        Data.List.NonEmpty.cons (align doc) (docs bL bR)
      where
        doc = diffExprB aL aR
    docs aL aR =
        diffExprB aL aR :| []
diffExprA l@(Annot {}) r =
    mismatch l r
diffExprA l r@(Annot {}) =
    mismatch l r
diffExprA l r =
    diffExprB l r

diffExprB :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprB l@(Lam {}) r@(Lam {}) =
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
            <>  format mempty (diffExprA bL bR)
            <>  rparen

    docs aL aR =
        pure (diffExprC aL aR)
diffExprB l@(Lam {}) r =
    mismatch l r
diffExprB l r@(Lam {}) =
    mismatch l r
diffExprB l@(BoolIf {}) r@(BoolIf {}) =
    enclosed' "      " (keyword "else" <> "  ") (docs l r)
  where
    docs (BoolIf aL bL cL) (BoolIf aR bR cR) =
        Data.List.NonEmpty.cons (align doc) (docs cL cR)
      where
        doc =   keyword "if"
            <>  " "
            <>  format " " (diffExprA aL aR)
            <>  keyword "then"
            <>  " "
            <>  diffExprA bL bR
    docs aL aR =
        pure (diffExprB aL aR)
diffExprB l@(BoolIf {}) r =
    mismatch l r
diffExprB l r@(BoolIf {}) =
    mismatch l r
diffExprB l@(Pi {}) r@(Pi {}) =
    enclosed' "  " (rarrow <> " ") (docs l r)
  where
    docs (Pi aL bL cL) (Pi aR bR cR) =
        Data.List.NonEmpty.cons (align doc) (docs cL cR)
      where
        doc =   forall
            <>  lparen
            <>  format " " (diffLabel aL aR)
            <>  colon
            <>  " "
            <>  format mempty (diffExprA bL bR)
            <>  rparen
    docs aL aR = pure (diffExprB aL aR)
diffExprB l@(Pi {}) r =
    mismatch l r
diffExprB l r@(Pi {}) =
    mismatch l r
diffExprB l@(Let {}) r@(Let {}) =
    enclosed' "    " (keyword "in" <> "  ") (docs l r)
  where
    docs (Let aL bL cL dL) (Let aR bR cR dR) =
        Data.List.NonEmpty.cons (align doc) (docs dL dR)
      where
        doc =   keyword "let"
            <>  " "
            <>  format " " (diffLabel aL aR)
            <>  format " " (diffMaybe (colon <> " ") diffExprA bL bR)
            <>  equals
            <>  " "
            <>  diffExprA cL cR
    docs aL aR = pure (diffExprB aL aR)
diffExprB l@(Let {}) r =
    mismatch l r
diffExprB l r@(Let {}) =
    mismatch l r
-- TODO: Implement proper list diff
diffExprB l@(ListLit {}) r@(ListLit {}) =
    mismatch l r
diffExprB l@(ListLit {}) r =
    mismatch l r
diffExprB l r@(ListLit {}) =
    mismatch l r
diffExprB (OptionalLit aL bL) (OptionalLit aR bR) = align doc
  where
    doc =   lbracket
        <>  " "
        <>  format " " (diffMaybe mempty diffExprA bL bR)
        <>  rbracket
        <>  " "
        <>  colon
        <>  " "
        <>  diffExprD (App Optional aL) (App Optional aR)
diffExprB l@(OptionalLit {}) r =
    mismatch l r
diffExprB l r@(OptionalLit {}) =
    mismatch l r
diffExprB (Merge aL bL cL) (Merge aR bR cR) = align doc
  where
    doc =   keyword "merge"
        <>  " "
        <>  format " " (diffExprE aL aR)
        <>  format " " (diffExprE bL bR)
        <>  diffMaybe (colon <> " ") diffExprE cL cR
diffExprB l@(Merge {}) r =
    mismatch l r
diffExprB l r@(Merge {}) =
    mismatch l r
diffExprB l r =
    diffExprC l r

diffExprC :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprC = diffBoolOr

diffBoolOr :: Pretty a => Expr s a -> Expr s a -> Diff
diffBoolOr l@(BoolOr {}) r@(BoolOr {}) =
    enclosed' "    " (operator "||" <> "  ") (docs l r)
  where
    docs (BoolOr aL bL) (BoolOr aR bR) =
        Data.List.NonEmpty.cons (diffTextAppend aL aR) (docs bL bR)
    docs aL aR =
        pure (diffTextAppend aL aR)
diffBoolOr l@(BoolOr {}) r =
    mismatch l r
diffBoolOr l r@(BoolOr {}) =
    mismatch l r
diffBoolOr l r =
    diffTextAppend l r

diffText :: Text -> Text -> Diff
diffText = diffPrimitive (token . Internal.prettyText)

diffTextAppend :: Pretty a => Expr s a -> Expr s a -> Diff
diffTextAppend l@(TextAppend {}) r@(TextAppend {}) =
    enclosed' "    " (operator "++" <> "  ") (docs l r)
  where
    docs (TextAppend aL bL) (TextAppend aR bR) =
        Data.List.NonEmpty.cons (diffNaturalPlus aL aR) (docs bL bR)
    docs aL aR =
        pure (diffNaturalPlus aL aR)
diffTextAppend l@(TextAppend {}) r =
    mismatch l r
diffTextAppend l r@(TextAppend {}) =
    mismatch l r
diffTextAppend l r =
    diffNaturalPlus l r

diffNaturalPlus :: Pretty a => Expr s a -> Expr s a -> Diff
diffNaturalPlus l@(NaturalPlus {}) r@(NaturalPlus {}) =
    enclosed' "  " (operator "+" <> " ") (docs l r)
  where
    docs (NaturalPlus aL bL) (NaturalPlus aR bR) =
        Data.List.NonEmpty.cons (diffListAppend aL aR) (docs bL bR)
    docs aL aR =
        pure (diffListAppend aL aR)
diffNaturalPlus l@(NaturalPlus {}) r =
    mismatch l r
diffNaturalPlus l r@(NaturalPlus {}) =
    mismatch l r
diffNaturalPlus l r =
    diffListAppend l r

diffListAppend :: Pretty a => Expr s a -> Expr s a -> Diff
diffListAppend l@(ListAppend {}) r@(ListAppend {}) =
    enclosed' "  " (operator "#" <> " ") (docs l r)
  where
    docs (ListAppend aL bL) (ListAppend aR bR) =
        Data.List.NonEmpty.cons (diffBoolAnd aL aR) (docs bL bR)
    docs aL aR =
        pure (diffBoolAnd aL aR)
diffListAppend l@(ListAppend {}) r =
    mismatch l r
diffListAppend l r@(ListAppend {}) =
    mismatch l r
diffListAppend l r =
    diffBoolAnd l r

diffBoolAnd :: Pretty a => Expr s a -> Expr s a -> Diff
diffBoolAnd l@(BoolAnd {}) r@(BoolAnd {}) =
    enclosed' "    " (operator "&&" <> "  ") (docs l r)
  where
    docs (BoolAnd aL bL) (BoolAnd aR bR) =
        Data.List.NonEmpty.cons (diffCombine aL aR) (docs bL bR)
    docs aL aR =
        pure (diffCombine aL aR)
diffBoolAnd l@(BoolAnd {}) r =
    mismatch l r
diffBoolAnd l r@(BoolAnd {}) =
    mismatch l r
diffBoolAnd l r =
    diffCombine l r

diffCombine :: Pretty a => Expr s a -> Expr s a -> Diff
diffCombine l@(Combine {}) r@(Combine {}) =
    enclosed' "  " (operator "∧" <> " ") (docs l r)
  where
    docs (Combine aL bL) (Combine aR bR) =
        Data.List.NonEmpty.cons (diffPrefer aL aR) (docs bL bR)
    docs aL aR =
        pure (diffPrefer aL aR)
diffCombine l@(Combine {}) r =
    mismatch l r
diffCombine l r@(Combine {}) =
    mismatch l r
diffCombine l r =
    diffPrefer l r

diffPrefer :: Pretty a => Expr s a -> Expr s a -> Diff
diffPrefer l@(Prefer {}) r@(Prefer {}) =
    enclosed' "  " (operator "⫽" <> " ") (docs l r)
  where
    docs (Prefer aL bL) (Prefer aR bR) =
        Data.List.NonEmpty.cons (diffCombineTypes aL aR) (docs bL bR)
    docs aL aR =
        pure (diffCombineTypes aL aR)
diffPrefer l@(Prefer {}) r =
    mismatch l r
diffPrefer l r@(Prefer {}) =
    mismatch l r
diffPrefer l r =
    diffCombineTypes l r

diffCombineTypes :: Pretty a => Expr s a -> Expr s a -> Diff
diffCombineTypes l@(CombineTypes {}) r@(CombineTypes {}) =
    enclosed' "  " (operator "*" <> " ") (docs l r)
  where
    docs (CombineTypes aL bL) (CombineTypes aR bR) =
        Data.List.NonEmpty.cons (diffNaturalTimes aL aR) (docs bL bR)
    docs aL aR =
        pure (diffNaturalTimes aL aR)
diffCombineTypes l@(CombineTypes {}) r =
    mismatch l r
diffCombineTypes l r@(CombineTypes {}) =
    mismatch l r
diffCombineTypes l r =
    diffNaturalTimes l r

diffNaturalTimes :: Pretty a => Expr s a -> Expr s a -> Diff
diffNaturalTimes l@(NaturalTimes {}) r@(NaturalTimes {}) =
    enclosed' "  " (operator "*" <> " ") (docs l r)
  where
    docs (NaturalTimes aL bL) (NaturalTimes aR bR) =
        Data.List.NonEmpty.cons (diffBoolEQ aL aR) (docs bL bR)
    docs aL aR =
        pure (diffBoolEQ aL aR)
diffNaturalTimes l@(NaturalTimes {}) r =
    mismatch l r
diffNaturalTimes l r@(NaturalTimes {}) =
    mismatch l r
diffNaturalTimes l r =
    diffBoolEQ l r

diffBoolEQ :: Pretty a => Expr s a -> Expr s a -> Diff
diffBoolEQ l@(BoolEQ {}) r@(BoolEQ {}) =
    enclosed' "    " (operator "==" <> "  ") (docs l r)
  where
    docs (BoolEQ aL bL) (BoolEQ aR bR) =
        Data.List.NonEmpty.cons (diffBoolNE aL aR) (docs bL bR)
    docs aL aR =
        pure (diffBoolNE aL aR)
diffBoolEQ l@(BoolEQ {}) r =
    mismatch l r
diffBoolEQ l r@(BoolEQ {}) =
    mismatch l r
diffBoolEQ l r =
    diffBoolNE l r

diffBoolNE :: Pretty a => Expr s a -> Expr s a -> Diff
diffBoolNE l@(BoolNE {}) r@(BoolNE {}) =
    enclosed' "    " (operator "!=" <> "  ") (docs l r)
  where
    docs (BoolNE aL bL) (BoolNE aR bR) =
        Data.List.NonEmpty.cons (diffExprD aL aR) (docs bL bR)
    docs aL aR =
        pure (diffExprD aL aR)
diffBoolNE l@(BoolNE {}) r =
    mismatch l r
diffBoolNE l r@(BoolNE {}) =
    mismatch l r
diffBoolNE l r =
    diffExprD l r

diffExprD :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprD l@(App {}) r@(App {}) =
    enclosed' mempty mempty (Data.List.NonEmpty.reverse (docs l r))
  where
    docs (App aL bL) (App aR bR) =
        Data.List.NonEmpty.cons (diffExprE bL bR) (docs aL aR)
    docs (Constructors aL) (Constructors aR) =
        diffExprE aL aR :| [ keyword "constructors" ]
    docs aL@(App {}) aR@(Constructors {}) =
        pure (mismatch aL aR)
    docs aL@(Constructors {}) aR@(App {}) =
        pure (mismatch aL aR)
    docs aL aR =
        pure (diffExprE aL aR)
diffExprD l@(App {}) r =
    mismatch l r
diffExprD l r@(App {}) =
    mismatch l r
diffExprD l@(Constructors {}) r@(Constructors {}) =
    enclosed' mempty mempty (keyword "constructors" :| [ diffExprE l r ])
diffExprD l@(Constructors {}) r =
    mismatch l r
diffExprD l r@(Constructors {}) =
    mismatch l r
diffExprD l r =
    diffExprE l r

diffExprE :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprE l@(Field {}) r@(Field {}) =
    enclosed' "  " (dot <> " ") (Data.List.NonEmpty.reverse (docs l r))
  where
    docs (Field aL bL) (Field aR bR) =
        Data.List.NonEmpty.cons (diffLabel bL bR) (docs aL aR)
    docs (Project aL bL) (Project aR bR) =
        Data.List.NonEmpty.cons (diffLabels bL bR) (docs aL aR)
    docs aL aR =
        pure (diffExprF aL aR)
diffExprE l@(Field {}) r =
    mismatch l r
diffExprE l r@(Field {}) =
    mismatch l r
diffExprE l@(Project {}) r@(Project {}) =
    enclosed' "  " (dot <> " ") (Data.List.NonEmpty.reverse (docs l r))
  where
    docs (Field aL bL) (Field aR bR) =
        Data.List.NonEmpty.cons (diffLabel bL bR) (docs aL aR)
    docs (Project aL bL) (Project aR bR) =
        Data.List.NonEmpty.cons (diffLabels bL bR) (docs aL aR)
    docs aL aR =
        pure (diffExprF aL aR)
diffExprE l@(Project {}) r =
    mismatch l r
diffExprE l r@(Project {}) =
    mismatch l r
diffExprE l r =
    diffExprF l r

diffExprF :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprF (Var aL) (Var aR) =
    diffVar aL aR
diffExprF l@(Var {}) r =
    mismatch l r
diffExprF l r@(Var {}) =
    mismatch l r
diffExprF (Const aL) (Const aR) =
    diffConst aL aR
diffExprF l@(Const {}) r =
    mismatch l r
diffExprF l r@(Const {}) =
    mismatch l r
diffExprF Bool Bool =
    "…"
diffExprF l@Bool r =
    mismatch l r
diffExprF l r@Bool =
    mismatch l r
diffExprF Natural Natural =
    "…"
diffExprF l@Natural r =
    mismatch l r
diffExprF l r@Natural =
    mismatch l r
diffExprF NaturalFold NaturalFold =
    "…"
diffExprF l@NaturalFold r =
    mismatch l r
diffExprF l r@NaturalFold =
    mismatch l r
diffExprF NaturalBuild NaturalBuild =
    "…"
diffExprF l@NaturalBuild r =
    mismatch l r
diffExprF l r@NaturalBuild =
    mismatch l r
diffExprF NaturalIsZero NaturalIsZero =
    "…"
diffExprF l@NaturalIsZero r =
    mismatch l r
diffExprF l r@NaturalIsZero =
    mismatch l r
diffExprF NaturalEven NaturalEven =
    "…"
diffExprF l@NaturalEven r =
    mismatch l r
diffExprF l r@NaturalEven =
    mismatch l r
diffExprF NaturalOdd NaturalOdd =
    "…"
diffExprF l@NaturalOdd r =
    mismatch l r
diffExprF l r@NaturalOdd =
    mismatch l r
diffExprF NaturalToInteger NaturalToInteger =
    "…"
diffExprF l@NaturalToInteger r =
    mismatch l r
diffExprF l r@NaturalToInteger =
    mismatch l r
diffExprF NaturalShow NaturalShow =
    "…"
diffExprF l@NaturalShow r =
    mismatch l r
diffExprF l r@NaturalShow =
    mismatch l r
diffExprF Integer Integer =
    "…"
diffExprF l@Integer r =
    mismatch l r
diffExprF l r@Integer =
    mismatch l r
diffExprF IntegerShow IntegerShow =
    "…"
diffExprF l@IntegerShow r =
    mismatch l r
diffExprF l r@IntegerShow =
    mismatch l r
diffExprF Double Double =
    "…"
diffExprF l@Double r =
    mismatch l r
diffExprF l r@Double =
    mismatch l r
diffExprF DoubleShow DoubleShow =
    "…"
diffExprF l@DoubleShow r =
    mismatch l r
diffExprF l r@DoubleShow =
    mismatch l r
diffExprF Text Text =
    "…"
diffExprF l@Text r =
    mismatch l r
diffExprF l r@Text =
    mismatch l r
diffExprF FilePath FilePath =
    "…"
diffExprF l@FilePath r =
    mismatch l r
diffExprF l r@FilePath =
    mismatch l r
diffExprF Url Url =
    "…"
diffExprF l@Url r =
    mismatch l r
diffExprF l r@Url =
    mismatch l r
diffExprF List List =
    "…"
diffExprF l@List r =
    mismatch l r
diffExprF l r@List =
    mismatch l r
diffExprF ListBuild ListBuild =
    "…"
diffExprF l@ListBuild r =
    mismatch l r
diffExprF l r@ListBuild =
    mismatch l r
diffExprF ListFold ListFold =
    "…"
diffExprF l@ListFold r =
    mismatch l r
diffExprF l r@ListFold =
    mismatch l r
diffExprF ListLength ListLength =
    "…"
diffExprF l@ListLength r =
    mismatch l r
diffExprF l r@ListLength =
    mismatch l r
diffExprF ListHead ListHead =
    "…"
diffExprF l@ListHead r =
    mismatch l r
diffExprF l r@ListHead =
    mismatch l r
diffExprF ListLast ListLast =
    "…"
diffExprF l@ListLast r =
    mismatch l r
diffExprF l r@ListLast =
    mismatch l r
diffExprF ListIndexed ListIndexed =
    "…"
diffExprF l@ListIndexed r =
    mismatch l r
diffExprF l r@ListIndexed =
    mismatch l r
diffExprF ListReverse ListReverse =
    "…"
diffExprF l@ListReverse r =
    mismatch l r
diffExprF l r@ListReverse =
    mismatch l r
diffExprF Optional Optional =
    "…"
diffExprF l@Optional r =
    mismatch l r
diffExprF l r@Optional =
    mismatch l r
diffExprF OptionalFold OptionalFold =
    "…"
diffExprF l@OptionalFold r =
    mismatch l r
diffExprF l r@OptionalFold =
    mismatch l r
diffExprF OptionalBuild OptionalBuild =
    "…"
diffExprF l@OptionalBuild r =
    mismatch l r
diffExprF l r@OptionalBuild =
    mismatch l r
diffExprF (BoolLit aL) (BoolLit aR) =
    diffBool aL aR
diffExprF l@(BoolLit {}) r =
    mismatch l r
diffExprF l r@(BoolLit {}) =
    mismatch l r
diffExprF (IntegerLit aL) (IntegerLit aR) =
    diffInteger aL aR
diffExprF l@(IntegerLit {}) r =
    mismatch l r
diffExprF l r@(IntegerLit {}) =
    mismatch l r
diffExprF (NaturalLit aL) (NaturalLit aR) =
    token (Internal.literal "+") <> diffNatural aL aR
diffExprF l@(NaturalLit {}) r =
    mismatch l r
diffExprF l r@(NaturalLit {}) =
    mismatch l r
diffExprF (DoubleLit aL) (DoubleLit aR) =
    diffScientific aL aR
diffExprF l@(DoubleLit {}) r =
    mismatch l r
diffExprF l r@(DoubleLit {}) =
    mismatch l r
-- TODO: Implement proper textual diff
diffExprF l@(TextLit {}) r@(TextLit {}) =
    mismatch l r
diffExprF l@(TextLit {}) r =
    mismatch l r
diffExprF l r@(TextLit {}) =
    mismatch l r
diffExprF (FilePathLit aL) (FilePathLit aR) =
    diffText (Text.pack aL) (Text.pack aR)
diffExprF l@(FilePathLit _) r =
    mismatch l r
diffExprF l r@(FilePathLit _) =
    mismatch l r
diffExprF (UrlLit aL) (UrlLit aR) =
    diffText aL aR
diffExprF l@(UrlLit _) r =
    mismatch l r
diffExprF l r@(UrlLit _) =
    mismatch l r
diffExprF (Record aL) (Record aR) =
    diffRecord aL aR
diffExprF l@(Record {}) r =
    mismatch l r
diffExprF l r@(Record {}) =
    mismatch l r
diffExprF (RecordLit aL) (RecordLit aR) =
    diffRecordLit aL aR
diffExprF l@(RecordLit {}) r =
    mismatch l r
diffExprF l r@(RecordLit {}) =
    mismatch l r
diffExprF (Union aL) (Union aR) =
    diffUnion aL aR
diffExprF l@(Union {}) r =
    mismatch l r
diffExprF l r@(Union {}) =
    mismatch l r
diffExprF (UnionLit aL bL cL) (UnionLit aR bR cR) =
    diffUnionLit aL aR bL bR cL cR
diffExprF l@(UnionLit {}) r =
    mismatch l r
diffExprF l r@(UnionLit {}) =
    mismatch l r
diffExprF aL aR =
    if same doc
    then ignore
    else align ("( " <> doc <> hardline <> ")")
  where
    doc = diffExprA aL aR
