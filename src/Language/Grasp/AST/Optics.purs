module Language.Grasp.AST.Optics where

import Prelude
import Data.Either (Either(..))
import Data.Lens
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST

getLabel :: ∀ a. NodeF a -> a
getLabel = unNode

getNodeLabel :: Node -> Type
getNodeLabel (Node (l /\ _)) = l

hyperEdgePrism :: Prism' GElem1 HyperEdge
hyperEdgePrism = prism GHyperEdge1 case _ of
  n@(GNode1 _)  -> Left n
  GHyperEdge1 e -> Right e

hyperEdgePrism'' :: Prism' GElem1 HyperEdge
hyperEdgePrism'' = prism' GHyperEdge1 case _ of
  GNode1 _      -> Nothing
  GHyperEdge1 e -> Just e

hyperEdgePrism' :: ∀ f v. Prism (GElem1F f v) (GElem1F f v) (HyperEdgeF f v) (HyperEdgeF f v)
hyperEdgePrism' = prism GHyperEdge1 case _ of
  n@(GNode1 _)  -> Left n
  GHyperEdge1 e -> Right e

_hyperEdgeFLabelPrism :: ∀ f a. Monoid (f (NodeF a)) => Prism' (HyperEdgeF f a) a
_hyperEdgeFLabelPrism = prism'
  (\l                       -> (HyperEdge (pure l) mempty mempty))
  (\(HyperEdge lMaybe _ _ ) -> lMaybe)
