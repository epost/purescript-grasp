module Language.Grasp.AST where

import Prelude

data Node = Node String

data Edge = Edge Node Node

instance nodeEq :: Eq Node where
  eq (Node x) (Node y) = x == y

instance nodeShow :: Show Node where
  show (Node x) = "(Node " <> x <> ")"

instance edgeEq :: Eq Edge where
  eq (Edge s1 t1) (Edge s2 t2) = s1 == s2 && t1 == t2

instance edgeShow :: Show Edge where
  show (Edge s t) = "(Edge " <> show s <> " " <> show t <> ")"
