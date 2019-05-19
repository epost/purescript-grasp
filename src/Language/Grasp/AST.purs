module Language.Grasp.AST where

import Prelude
import Data.List (List(..))
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldMap)

type Label = String

type LabelAndType = Label /\ (Maybe Type)

type Type = String

data Node = Node LabelAndType

data Edge = Edge (Maybe LabelAndType) Node Node

data MultiEdge = MultiEdge (Maybe LabelAndType) (List Node) (List Node)

instance nodeEq :: Eq Node where
  eq (Node x) (Node y) = x == y

instance nodeShow :: Show Node where
  show (Node (label /\ typ)) = "(Node " <> label <> (foldMap (append ":") typ) <> ")"

instance edgeEq :: Eq Edge where
  eq (Edge l1 s1 t1) (Edge l2 s2 t2) = l1 == l2 && s1 == s2 && t1 == t2

instance edgeShow :: Show Edge where
  show (Edge l s t) = "(Edge " <> show l <> " " <> show s <> " " <> show t <> ")"

instance multiEdgeShow :: Show MultiEdge where
  show (MultiEdge l s t) = "(MultiEdge " <> show l <> " " <> show s <> " " <> show t <> ")"

instance multiEdgeEq :: Eq MultiEdge where
  eq (MultiEdge l1 s1 t1) (MultiEdge l2 s2 t2) = l1 == l2 && s1 == s2 && t1 == t2

unNode :: Node -> LabelAndType
unNode (Node x) = x

--------------------------------------------------------------------------------

data GElem1 = GNode1 Node
            | GEdge1 Edge
            | GMultiEdge1 MultiEdge

instance gelem1Eq :: Eq GElem1 where
  eq (GNode1 x) (GNode1 y) = x == y
  eq (GEdge1 x) (GEdge1 y) = x == y
  eq _          _          = false

instance gelem1Show :: Show GElem1 where
  show (GNode1 x)      = "(GNode1 "      <> show x <> ")"
  show (GEdge1 x)      = "(GEdge1 "      <> show x <> ")"
  show (GMultiEdge1 x) = "(GMultiEdge1 " <> show x <> ")"
