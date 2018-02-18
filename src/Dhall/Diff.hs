{-# LANGUAGE OverloadedStrings #-}

module Dhall.Diff where

import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Monoid (Any(..), (<>))
import Data.Text.Lazy (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Dhall.Core (Expr(..))
import Dhall.Pretty.Internal (Ann)

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Set
import qualified Data.Text.Prettyprint.Doc  as Pretty
import qualified Dhall.Core
import qualified Dhall.Pretty.Internal      as Internal

minus :: Doc ann -> Doc ann
minus doc = "- " <> doc

plus :: Doc ann -> Doc ann
plus doc = "+ " <> doc

diff :: (Eq a, Eq s, Pretty a) => Expr s a -> Expr s a -> Doc Ann
diff l r = Pretty.align (go (Dhall.Core.denote l) (Dhall.Core.denote r))
  where
    go :: (Eq a, Pretty a) => Expr () a -> Expr () a -> Doc Ann
    go (Pi aL bL cL) (Pi aR bR cR)
        | aL == aR
    go (Record kvsL) (Record kvsR) =
        braced (diffKeyVals Internal.colon kvsL kvsR)
    go (RecordLit kvsL) (RecordLit kvsR) =
        braced (diffKeyVals Internal.equals kvsL kvsR)
    go (Union kvsL) (Union kvsR) =
        angled (diffKeyVals Internal.colon kvsL kvsR)
    go (UnionLit kL vL kvsL) (UnionLit kR vR kvsR) =
            enclosure0 (diffKeyVals Internal.equals kvsL0 kvsR0)
        <>  enclosure  (diffKeyVals Internal.colon  kvsL  kvsR )
      where
        kvsL0 = HashMap.singleton kL vL
        kvsR0 = HashMap.singleton kR vR

        middle =
            if HashMap.null kvsL && HashMap.null kvsR
            then mempty
            else Internal.pipe

        enclosure0 = enclosed Internal.langle mempty mempty
        enclosure  = enclosed middle Internal.pipe Internal.rangle
    go exprL exprR =
            minus (Internal.prettyExpr exprL)
        <>  Pretty.hardline
        <>  plus  (Internal.prettyExpr exprR)

enclosed :: Doc Ann -> Doc Ann -> Doc Ann -> [Doc Ann] -> Doc Ann
enclosed l _ r [] = l <> r
enclosed l m r docs = mconcat (zipWith (<>) prefixes docs) <> suffix
  where
    prefixes = (l <> " ") : repeat (Pretty.hardline <> m <> " ")

    suffix = Pretty.hardline <> r

braced :: [Doc Ann] -> Doc Ann
braced = enclosed Internal.lbrace Internal.comma Internal.rbrace

angled :: [Doc Ann] -> Doc Ann
angled = enclosed Internal.langle Internal.pipe Internal.rangle

diffKeyVals
    :: (Eq a, Eq s, Pretty a)
    => Doc Ann
    -> InsOrdHashMap Text (Expr s a)
    -> InsOrdHashMap Text (Expr s a)
    -> [Doc Ann]
diffKeyVals assign kvsL kvsR =
    diffFieldNames <> diffFieldValues <> (if anyEqual then [ "…" ] else [])
  where
    ksL = Data.Set.fromList (HashMap.keys kvsL)
    ksR = Data.Set.fromList (HashMap.keys kvsR)

    extraL = Data.Set.difference ksL ksR
    extraR = Data.Set.difference ksR ksL

    diffFieldNames = foldMap (adapt minus) extraL <> foldMap (adapt plus) extraR
      where
        adapt sign key =
            [   sign (Internal.label (Pretty.pretty key))
            <>  " "
            <>  assign
            <>  " …"
            ]

    shared = HashMap.intersectionWith (,) kvsL kvsR

    diffFieldValues = HashMap.foldMapWithKey adapt shared
      where
        adapt key (exprL, exprR)
            | exprL == exprR = []
            | otherwise =
                [   (if ksL == ksR then mempty else "  ")
                <>  Internal.label (Pretty.pretty key)
                <>  " "
                <>  assign
                <>  " "
                <>  diff exprL exprR
                ]

    anyEqual = getAny (foldMap adapt shared)
      where
        adapt (exprL, exprR) = Any (exprL == exprR)
