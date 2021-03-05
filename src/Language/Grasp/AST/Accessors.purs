module Language.Grasp.AST.Accessors where

import Prelude
import Data.Either (Either(..))
import Data.Lens
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST

getGElem1Node :: ∀ f a. GElem1F f a -> Maybe (NodeF a)
getGElem1Node = case _ of
  GNode1 n      -> Just n
  GHyperEdge1 e -> Nothing

getGElem1HyperEdge :: ∀ f a. GElem1F f a -> Maybe (HyperEdgeF f a)
getGElem1HyperEdge = case _ of
  GNode1 _      -> Nothing
  GHyperEdge1 e -> Just e

getGElem1HyperEdgeNodes :: ∀ f a. GElem1F f a -> Maybe (f (NodeF a) /\ f (NodeF a))
getGElem1HyperEdgeNodes = case _ of
  GNode1 _      -> Nothing
  GHyperEdge1 e -> Just (nodesFromHyperEdge e)

-- TODO naming: sourcesAndTargets? edgeNodes?
nodesFromHyperEdge :: forall f a. HyperEdgeF f a -> f (NodeF a) /\ f (NodeF a)
nodesFromHyperEdge (HyperEdge _ s t) = s /\ t
