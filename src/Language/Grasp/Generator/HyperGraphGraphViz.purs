module Language.Grasp.Generator.HyperGraphGraphViz where

import Prelude
import Data.Foldable (fold)
import Data.List as List
import Data.List (List(..), elem)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Language.Grasp.AST
import Language.Grasp.Generator
import Language.Grasp.Stylesheet.AST as Stylesheet
import Language.Grasp.Generator.GraphViz as GraphViz
import Language.Grasp.Generator.HyperGraph as HyperGraph

-- | We interpret the given graph as a hypergraph by doing some transformations on the hyperedges
-- | and then we hand the transformed graph off to GraphViz for visualisation.
digraph :: List GElem1 -> Styler -> String
digraph g styler =
  GraphViz.digraph flattenedHyperGraph syntheticNodeStyler
  where
    flattenedHyperGraph = List.fromFoldable $ HyperGraph.interpretAsHypergraph2 g

    syntheticNodeStyler :: Styler
    syntheticNodeStyler sel = case List.uncons sel of
      Just { head: selHead@(Stylesheet.SNode nodeId), tail: Nil } | selHead `elem` syntheticNodeSelectors -> Just (mkSyntheticNodeStyle nodeId <> fold (styler sel))
      _                                                                                                   -> styler sel
      where
        syntheticNodeSelectors :: List Stylesheet.SelectorElem
        syntheticNodeSelectors = HyperGraph.hyperEdgeSelectors g


mkSyntheticNodeStyle :: Label -> List GraphViz.GraphVizAttr
mkSyntheticNodeStyle label = List.fromFoldable
  [ "background" /\ "goldenrod2"
  , "shape"      /\ "square"
  , "label"      /\ ""
  , "xlabel"     /\ label
  ]
