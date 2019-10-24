module Language.Grasp.AST where

import Prelude
import Data.List (List(..))
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldMap)

type Label = String

type LabelAndType = Label /\ (Maybe Type)

type Type = String

data NodeF a = Node a
type Node = NodeF LabelAndType

data MultiEdgeF f l s t = MultiEdge (Maybe l) (f (NodeF s)) (f (NodeF t))
type MultiEdge          = MultiEdgeF List LabelAndType LabelAndType LabelAndType

derive instance eqNodeF :: Eq a => Eq (NodeF a)

instance showNodeF :: Show a => Show (NodeF a) where
  show (Node x) = "(NodeF " <> show x <> ")"

instance showMultiEdgeF :: (Show l, Show s, Show t) => Show (MultiEdgeF List l s t) where
  show (MultiEdge l s t) = "(MultiEdge " <> show l <> " " <> show s <> " " <> show t <> ")"

derive instance eqMultiEdgeF :: (Eq l, Eq s, Eq t) => Eq (MultiEdgeF List l s t)

unNode :: forall f a. NodeF a -> a
unNode (Node x) = x

--------------------------------------------------------------------------------

data GElem1F a = GNode1      (NodeF a)
               | GMultiEdge1 (MultiEdgeF List a a a)

-- | A graph can be be represented by `List GElem1`.
type GElem1 = GElem1F LabelAndType

derive instance eqGElem1 :: Eq a => Eq (GElem1F a)

instance showGElem1 :: Show a => Show (GElem1F a) where
  show (GNode1 x)      = "(GNode1 "      <> show x <> ")"
  show (GMultiEdge1 x) = "(GMultiEdge1 " <> show x <> ")"
