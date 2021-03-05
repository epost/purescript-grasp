module Language.Grasp.AST.Util where

import Prelude
import Data.Compactable (class Compactable, compact)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST
import Language.Grasp.AST.Optics
import Language.Grasp.AST.Accessors


-- Some helpers based on simpler accessors -------------------------------------

-- TODO Using `compact` instead of `catMaybes` requires an extra lib, so maybe not worth it. Otoh, it is nicely general.
nodeElemNodes :: forall f a. Functor f => Compactable f => f (GElem1F f a) -> f (NodeF a)
nodeElemNodes elems = elems # map getGElem1Node >>> compact

edgeSourceAndTargetNodes :: forall f g a. Functor f => Compactable f => Functor g => f (GElem1F g a) -> f (g (NodeF a) /\ g (NodeF a))
edgeSourceAndTargetNodes elems = elems # map getGElem1HyperEdgeNodes >>> compact

edgeSourceAndTargetNodesSmashedTogether
  :: ∀ f g a
   . Functor f => Compactable f => Functor g => Semigroup (g (NodeF a))
  => f (GElem1F g a) -> f (g (NodeF a))
edgeSourceAndTargetNodesSmashedTogether elems = elems # map (getGElem1HyperEdgeNodes >>> (map (uncurry (<>)))) >>> compact
