-- graph elem kinds:
--
-- graph
-- edge
-- node
-- type?
-- arg? param?
-- subgraph?
-- arrow?

module Language.Grasp.Stylesheet.AST where

import Prelude

import Data.Tuple.Nested (type (/\))
import Data.List (List)
import Language.Grasp.AST (Label)

type Stylesheet = List (Selector /\ List Attr)

type Key = String

type Val = String

type Attr = Key /\ Val

type Attrs = List Attr

type NodeId = Label

type EdgeId = Label

-- TODO add selectors for node groups, all edges, all nodes, etc.
data SelectorElem
  = SNode NodeId
  | SEdge EdgeId

type Selector = List SelectorElem

instance showSelectrElem :: Show SelectorElem where
  show = case _ of
    SNode x -> "(SNode " <> x <> ")"
    SEdge x -> "(SEdge " <> x <> ")"

derive instance eqSelectorElem :: Eq SelectorElem

derive instance ordSelectorElem :: Ord SelectorElem
