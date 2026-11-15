-- | Canonical and presentation normal forms.
--
-- A canonical graph is the semantic normal form: hashing, the semantic cache,
-- and other program consumers use it. A presentation graph is that form after
-- common-subexpression elimination, and it is the only normal form meant to
-- be printed. 'normalizeToExprLegacy' rebuilds an 'Expr' and is a compatibility
-- boundary, not a semantic result.
module Dhall.Core.NormalForm
    ( CanonicalGraph (..)
    , PresentationGraph (..)
    , normalizeToGraph
    , normalizeWithConfigToGraph
    , toPresentationGraph
    , presentationExpr
    , alphaCanonical
    , normalizeToExprLegacy
    ) where

import Data.Void (Void)
import Dhall.Core.CSE (cseGraph)
import Dhall.Syntax (Expr)
import Prettyprinter (Pretty)

import qualified Dhall.Core.DAG as DAG
import qualified Dhall.Eval     as Eval
import qualified Dhall.Eval.Run as Run

-- | Semantic β-normal form. Source annotations are gone.
newtype CanonicalGraph a = CanonicalGraph
    { getCanonicalGraph :: DAG.CoreGraph a
    }

-- | CSE-reduced form of a canonical graph. For printing, not for hashing.
newtype PresentationGraph a = PresentationGraph
    { getPresentationGraph :: DAG.CoreGraph a
    }

normalizeToGraph :: Eq a => Expr s a -> CanonicalGraph a
normalizeToGraph = CanonicalGraph . Eval.normalizeGraph

normalizeWithConfigToGraph
    :: Eq a => Run.EvalConfig -> Expr s a -> CanonicalGraph a
normalizeWithConfigToGraph cfg =
    CanonicalGraph . Eval.normalizeGraphWithConfig cfg

toPresentationGraph
    :: (Eq a, Pretty a) => CanonicalGraph a -> PresentationGraph a
toPresentationGraph (CanonicalGraph g) = PresentationGraph (cseGraph g)

-- | Compact expression for the existing pretty-printer. The lets are already
-- in the graph, so this is not the expanded normal form.
presentationExpr :: (Eq a, Pretty a) => CanonicalGraph a -> Expr Void a
presentationExpr = DAG.toSharedExpr . getPresentationGraph . toPresentationGraph

alphaCanonical :: Eq a => CanonicalGraph a -> CanonicalGraph a
alphaCanonical (CanonicalGraph g) = CanonicalGraph (DAG.alphaNormalizeGraph g)

-- | Compatibility wrapper. Do not use this for a result a caller will print
-- or hash.
normalizeToExprLegacy :: Eq a => Expr s a -> Expr Void a
normalizeToExprLegacy = DAG.toSharedExpr . getCanonicalGraph . normalizeToGraph
