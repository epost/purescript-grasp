module Language.Grasp.AST.Optics where

import Prelude
import Data.Either (Either(..))
import Data.Lens
import Data.Tuple.Nested ((/\))
import Language.Grasp.AST

getLabel :: forall a. NodeF a -> a
getLabel = unNode

getNodeLabel :: Node -> Type
getNodeLabel (Node (l /\ _)) = l

hyperEdgeFocus :: Prism' GElem1 HyperEdge
hyperEdgeFocus = prism GHyperEdge1 case _ of
  n@(GNode1 _)  -> Left n
  GHyperEdge1 e -> Right e

hyperEdgeFocus' :: ∀ f v. Prism (GElem1F f v) (GElem1F f v) (HyperEdgeF f v) (HyperEdgeF f v)
hyperEdgeFocus' = prism GHyperEdge1 case _ of
  n@(GNode1 _)  -> Left n
  GHyperEdge1 e -> Right e

_hyperEdgeFLabelPrism :: ∀ f a. Monoid (f (NodeF a)) => Prism' (HyperEdgeF f a) a
_hyperEdgeFLabelPrism = prism'
  (\l                       -> (HyperEdge (pure l) mempty mempty))
  (\(HyperEdge lMaybe _ _ ) -> lMaybe)
