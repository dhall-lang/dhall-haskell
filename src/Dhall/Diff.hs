{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Diff where

import Data.Foldable (fold)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Monoid (Any(..), (<>))
import Data.Scientific (Scientific)
import Data.String (IsString(..))
import Data.Text.Lazy (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Data.List.NonEmpty (NonEmpty(..))
import Dhall.Core (Const(..), Expr(..), Var(..))
import Dhall.Pretty.Internal (Ann)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List.NonEmpty
import qualified Data.Set
import qualified Data.Text.Prettyprint.Doc  as Pretty
import qualified Dhall.Core
import qualified Dhall.Pretty.Internal      as Internal

data Diff =
    Diff
        { same :: Bool
        , doc  :: Doc Ann
        }

instance Monoid (Diff) where
    mempty = Diff {..}
      where
        same = True

        doc = mempty

    mappend (Diff sameL docL) (Diff sameR docR) =
        Diff (sameL && sameR) (docL <> docR)

instance IsString (Diff) where
    fromString string = Diff {..}
      where
        same = True

        doc = fromString string

ignore :: Diff
ignore = "…"

warn :: Diff
warn = "✗"

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

colon :: Diff
colon = token Internal.colon

comma :: Diff
comma = token Internal.comma

dot :: Diff
dot = token Internal.dot

if_ :: Diff
if_ = token (Internal.keyword "if")

then_ :: Diff
then_ = token (Internal.keyword "then")

else_ :: Diff
else_ = token (Internal.keyword "else")

forall :: Diff
forall = token Internal.forall

lambda :: Diff
lambda = token Internal.lambda

langle :: Diff
langle = token Internal.langle

lbrace :: Diff
lbrace = token Internal.lbrace

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

rparen :: Diff
rparen = token Internal.rparen

diff :: (Eq a, Pretty a) => Expr s a -> Expr s a -> Doc Ann
diff l0 r0 = doc
  where
    l1 = Dhall.Core.alphaNormalize (Dhall.Core.normalize l0)
    r1 = Dhall.Core.alphaNormalize (Dhall.Core.normalize r0)

    Diff {..} = diffExprA l1 r1

diffPrimitive :: Eq a => (a -> Diff) -> a -> a -> Diff
diffPrimitive f l r
    | l == r    = ignore
    | otherwise = difference (f l) (f r)

diffLabel :: Text -> Text -> Diff
diffLabel = diffPrimitive (token . Internal.prettyLabel)

diffNatural :: Natural -> Natural -> Diff
diffNatural = diffPrimitive (token . Internal.prettyNatural)

diffScientific :: Scientific -> Scientific -> Diff
diffScientific = diffPrimitive (token . Internal.prettyScientific)

diffConst :: Const -> Const -> Diff
diffConst = diffPrimitive (token . Internal.prettyConst)

diffBool :: Bool -> Bool -> Diff
diffBool = diffPrimitive bool
  where
    bool True  = token (Internal.builtin "True")
    bool False = token (Internal.builtin "False")

diffInteger :: Integer -> Integer -> Diff
diffInteger = diffPrimitive (token . Internal.prettyNumber)

diffVar :: Var -> Var -> Diff
diffVar (V xL nL) (V xR nR) =
    decide (same label) (same natural)
  where
    label = diffLabel xL xR

    natural = diffInteger nL nR

    decide True  True  =
        label <> "@" <> natural
    decide True  False =
        label <> "@" <> natural
    decide False True  =
        align
            (   "  "
            <>  label
            <>  hardline
            <>  "@ "
            <>  natural
            )
    decide False False =
        align
            (   "  "
            <>  label
            <>  hardline
            <>  "@ "
            <>  natural
            )

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
braced = enclosed (lbrace <> " ") (comma <> " ") (rbrace <> " ")

angled :: [Diff] -> Diff
angled = enclosed (langle <> " ") (pipe <> " ") (rangle <> " ")

diffRecord
    :: Pretty a
    => InsOrdHashMap Text (Expr s a) -> InsOrdHashMap Text (Expr s a) -> Diff
diffRecord kvsL kvsR = braced (diffKeyVals colon kvsL kvsR)

mismatch :: Pretty a => Expr s a -> Expr s a -> Diff
mismatch l r = difference (token (Pretty.pretty l)) (token (Pretty.pretty r))

diffExprA :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprA l@(Annot _ _) r@(Annot _ _) =
    enclosed' "  " (colon <> " ") (docs l r)
  where
    docs (Annot aL bL) (Annot aR bR) =
        Data.List.NonEmpty.cons (diffExprB aL aR) (docs bL bR)
    docs aL aR =
        diffExprB aL aR :| []
diffExprA l@(Annot _ _) r =
    mismatch l r
diffExprA l r@(Annot _ _) =
    mismatch l r
diffExprA l r =
    diffExprB l r

diffExprB :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprB l@(Lam _ _ _) r@(Lam _ _ _) =
    enclosed' "  " (rarrow <> " ") (docs l r)
  where
    docs (Lam aL bL cL) (Lam aR bR cR) =
        Data.List.NonEmpty.cons doc (docs cL cR)
      where
        label = diffLabel aL aR

        type_ = diffExprA bL bR

        doc = align (decide (same label) (same type_))

        decide True True =
                lambda
            <>  lparen
            <>  label
            <>  " "
            <>  colon
            <>  " "
            <>  type_
            <>  rparen
        decide True False =
                lambda
            <>  lparen
            <>  label
            <>  " "
            <>  colon
            <>  " "
            <>  type_
            <>  hardline
            <>  rparen
        decide False True =
                lambda
            <>  lparen
            <>  label
            <>  hardline
            <>  colon
            <>  " "
            <>  type_
            <>  rparen
        decide False False =
                lambda
            <>  lparen
            <>  label
            <>  hardline
            <>  colon
            <>  " "
            <>  type_
            <>  hardline
            <>  rparen
    docs aL aR = pure (diffExprC aL aR)
diffExprB l@(Lam _ _ _) r =
    mismatch l r
diffExprB l r@(Lam _ _ _) =
    mismatch l r
diffExprB l@(BoolIf _ _ _) r@(BoolIf _ _ _) =
    enclosed' "      " (else_ <> "  ") (docs l r)
  where
    docs (BoolIf aL bL cL) (BoolIf aR bR cR) =
        Data.List.NonEmpty.cons (decide (same predicate)) (docs cL cR)
      where
        predicate = diffExprA aL aR

        branch = diffExprA bL bR

        decide True =
                if_
            <>  " "
            <>  predicate
            <>  " "
            <>  then_
            <>  " "
            <>  branch
        decide False =
                if_
            <>  predicate
            <>  hardline
            <>  then_
            <>  " "
            <>  branch
    docs aL aR =
        pure (diffExprB aL aR)
diffExprB l@(BoolIf _ _ _) r =
    mismatch l r
diffExprB l r@(BoolIf _ _ _) =
    mismatch l r
diffExprB l@(Pi _ _ _) r@(Pi _ _ _) =
    enclosed' "  " (rarrow <> " ") (docs l r)
  where
    docs (Pi aL bL cL) (Pi aR bR cR) =
        Data.List.NonEmpty.cons doc (docs cL cR)
      where
        label = diffLabel aL aR

        type_ = diffExprA bL bR

        doc = align (decide (same label) (same type_))

        decide True True =
                forall
            <>  lparen
            <>  label
            <>  " "
            <>  colon
            <>  " "
            <>  type_
            <>  rparen
        decide True False =
                forall
            <>  lparen
            <>  label
            <>  " "
            <>  colon
            <>  " "
            <>  type_
            <>  hardline
            <>  rparen
        decide False True =
                forall
            <>  lparen
            <>  label
            <>  hardline
            <>  colon
            <>  " "
            <>  type_
            <>  rparen
        decide False False =
                forall
            <>  lparen
            <>  label
            <>  hardline
            <>  colon
            <>  " "
            <>  type_
            <>  hardline
            <>  rparen
    docs aL aR = pure (diffExprB aL aR)
diffExprB l@(Pi _ _ _) r =
    mismatch l r
diffExprB l r@(Pi _ _ _) =
    mismatch l r
-- TODO
diffExprB l r =
    diffExprC l r

-- TODO:
diffExprC :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprC l r =
    diffExprD l r

-- TODO: Support `constructors`
diffExprD :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprD l@(App _ _) r@(App _ _) =
    enclosed' mempty mempty (Data.List.NonEmpty.reverse (docs l r))
  where
    docs (App aL bL) (App aR bR) =
        Data.List.NonEmpty.cons (diffExprE bL bR) (docs aL aR)
    docs aL aR =
        pure (diffExprE aL aR)
diffExprD l@(App _ _) r =
    mismatch l r
diffExprD l r@(App _ _) =
    mismatch l r
diffExprD l r =
    diffExprE l r

diffExprE :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprE l@(Field _ _) r@(Field _ _) =
    enclosed' "  " (dot <> " ") (Data.List.NonEmpty.reverse (docs l r))
  where
    docs (Field aL bL) (Field aR bR) =
        Data.List.NonEmpty.cons (diffLabel bL bR) (docs aL aR)
    docs aL aR =
        pure (diffExprF aL aR)
diffExprE l@(Field _ _) r =
    mismatch l r
diffExprE l r@(Field _ _) =
    mismatch l r
diffExprE l r =
    diffExprF l r

diffExprF :: Pretty a => Expr s a -> Expr s a -> Diff
diffExprF (Var aL) (Var aR) =
    diffVar aL aR
diffExprF l@(Var _) r =
    mismatch l r
diffExprF l r@(Var _) =
    mismatch l r
diffExprF (Const aL) (Const aR) =
    diffConst aL aR
diffExprF l@(Const _) r =
    mismatch l r
diffExprF l r@(Const _) =
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
diffExprF l@(BoolLit _) r =
    mismatch l r
diffExprF l r@(BoolLit _) =
    mismatch l r
diffExprF (IntegerLit aL) (IntegerLit aR) =
    diffInteger aL aR
diffExprF l@(IntegerLit _) r =
    mismatch l r
diffExprF l r@(IntegerLit _) =
    mismatch l r
diffExprF (NaturalLit aL) (NaturalLit aR) =
    token (Internal.literal "+") <> diffNatural aL aR
diffExprF l@(NaturalLit _) r =
    mismatch l r
diffExprF l r@(NaturalLit _) =
    mismatch l r
diffExprF (DoubleLit aL) (DoubleLit aR) =
    diffScientific aL aR
diffExprF l@(DoubleLit _) r =
    mismatch l r
diffExprF l r@(DoubleLit _) =
    mismatch l r
-- TODO
-- diffExprF (TextLit aL) (TextLit aR) =
diffExprF (Record aL) (Record aR) =
    diffRecord aL aR
diffExprF l@(Record _) r =
    mismatch l r
diffExprF l r@(Record _) =
    mismatch l r
diffExprF aL aR =
    if same doc
    then ignore
    else align ("( " <> doc <> hardline <> ")")
  where
    doc = diffExprA aL aR
