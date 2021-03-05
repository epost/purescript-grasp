-- | A Grasp graph can have arrows of the form `a,b,c -> d,e`. These could be
-- | interpreted in several ways, including:
-- |
-- | 1) As multigraph arrows `a->d`, `a->e`, `b->c`, etc. This is what GraphViz does.
-- |
-- | 2) As single hyperedge from `a` and `b` to `c`,`d` and `e`.
-- |
-- | This generator interprets a Grasp graph as a hypergraph according to (2).
-- |
-- | For exampe, the hyperedge:
-- |
-- | ```
-- | a,b,c  -f->  d,e
-- | ```
-- |
-- | is transformed by flattening it into 3 inbound and 2 outbound non-hyperedges,
-- | with a synthetic vertex `f` inbetween:
-- |
-- | ```
-- | a          __> d
-- |   \_>     /
-- | b -->  f
-- |    _>     \__> e
-- | c /
-- | ```
-- |
-- | As long as there are no arrows between regular nodes and synthetic nodes, the
-- | resulting 'flattened' graph will be bipartite, with one partition containing
-- | the normal nodes, and the other containing the synthetic ones.
module Language.Grasp.Generator.HyperGraph where

import Prelude
import Data.Lens (preview, _1)
import Data.List as List
import Data.List (List)
import Data.Maybe (fromMaybe)
import Data.Foldable (class Foldable, intercalate, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST
import Language.Grasp.AST.Optics
import Language.Grasp.Generator
import Language.Grasp.Stylesheet.AST as Stylesheet
import Language.Grasp.Stylesheet.AST (NodeId, SelectorElem(..))


-- | Edge label to use in case none was given. We'll probably want something a bit nicer than this.
defaultEdgeName_HACK = "TODO"

interpretAsHypergraph :: forall f. Functor f => Foldable f => f GElem1 -> f (Maybe FlattenedHyperEdge)
interpretAsHypergraph g = flattenHyperEdges <$> g

flattenHyperEdges :: GElem1 -> Maybe FlattenedHyperEdge
flattenHyperEdges = case _ of
  GNode1 n      -> Nothing
  GHyperEdge1 e -> pure $ flattenHyperEdge e

--------------------------------------------------------------------------------

-- | Select the synthetic nodes that correspond to hyperedges in the input graph.
hyperEdgeSelectors :: List GElem1 -> List SelectorElem
hyperEdgeSelectors = List.mapMaybe (map Stylesheet.SNode <<< preview (_1 >>> _hyperEdgeFLabelPrism >>> hyperEdgePrism))

--------------------------------------------------------------------------------

interpretAsHypergraph2 :: List GElem1 -> List GElem1
interpretAsHypergraph2 g = flattenHyperEdges2 =<< g

flattenHyperEdges2 :: GElem1 -> List GElem1
flattenHyperEdges2 = case _ of
  n@(GNode1 _)      -> pure n
  GHyperEdge1 e     -> flattenHyperEdge' $ flattenHyperEdge e

flattenHyperEdge' :: FlattenedHyperEdgeF List LabelAndType -> List GElem1
flattenHyperEdge' fhe =
  (GNode1 fhe.node # pure) <>
  (GHyperEdge1 <<< toHyperEdge <$> fhe.inbound) <>
  (GHyperEdge1 <<< toHyperEdge <$> fhe.outbound)

flattenHyperEdge :: HyperEdgeF List LabelAndType -> FlattenedHyperEdgeF List LabelAndType
flattenHyperEdge (HyperEdge lMaybe srcNodes targetNodes) =
  { node:     syntheticNode
  , inbound:  (\srcNode    -> mkNonHyperEdge Nothing srcNode       syntheticNode) <$> srcNodes
  , outbound: (\targetNode -> mkNonHyperEdge Nothing syntheticNode targetNode)    <$> targetNodes
  }
  where
    syntheticNode = Node syntheticNodeLabelAndType
    syntheticNodeLabelAndType = lMaybe # fromMaybe (defaultEdgeName_HACK /\ Nothing)

--------------------------------------------------------------------------------

type FlattenedHyperEdgeF f a =
  { node     :: NodeF a
  , inbound  :: f (NonHyperEdgeF a)
  , outbound :: f (NonHyperEdgeF a)
  }

type FlattenedHyperEdge = FlattenedHyperEdgeF List LabelAndType
