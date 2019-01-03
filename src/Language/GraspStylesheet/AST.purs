module Language.Grasp.Stylesheet.AST where

import Prelude

import Data.Tuple (Tuple(..))
import Data.List (List)
import Language.Grasp.AST (Label)

type Key = String

type Val = String

type Attr = Tuple Key Val

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
