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
