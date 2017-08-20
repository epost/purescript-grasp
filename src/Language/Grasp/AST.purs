module Language.Grasp.AST where

import Prelude
import Data.Maybe

type Label = String

data Node = Node Label

data Edge = Edge (Maybe Label) Node Node

instance nodeEq :: Eq Node where
  eq (Node x) (Node y) = x == y

instance nodeShow :: Show Node where
  show (Node x) = "(Node " <> x <> ")"

instance edgeEq :: Eq Edge where
  eq (Edge l1 s1 t1) (Edge l2 s2 t2) = l1 == l2 && s1 == s2 && t1 == t2

instance edgeShow :: Show Edge where
  show (Edge l s t) = "(Edge (" <> show l <> ")" <> show s <> " " <> show t <> ")"

--------------------------------------------------------------------------------

data GElem1 = GNode1 Node
            | GEdge1 Edge

instance gelem1Eq :: Eq GElem1 where
  eq (GNode1 x) (GNode1 y) = x == y
  eq (GEdge1 x) (GEdge1 y) = x == y
  eq _          _          = false

instance gelem1Show :: Show GElem1 where
  show (GNode1 x) = "(GNode1 " <> show x <> ")"
  show (GEdge1 x) = "(GEdge1 " <> show x <> ")"
