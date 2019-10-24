module Language.Grasp.AST where

import Prelude
import Data.List (List(..))
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldMap)

type Label = String

-- | This captures labels with (type) annotations, which ay look something like `x:a`.
type LabelAndType = Label /\ (Maybe Type)

type Type = String

type Node = NodeF LabelAndType

type MultiEdge = MultiEdgeF List LabelAndType LabelAndType LabelAndType

-- | Graph element type. A graph is a bunch of nodes and edges, and can be be
-- | represented as a `List GElem1` for example.
type GElem1 = GElem1F LabelAndType

--------------------------------------------------------------------------------

newtype NodeF a = Node a

unNode :: forall f a. NodeF a -> a
unNode (Node x) = x

derive instance eqNodeF :: Eq a => Eq (NodeF a)

instance showNodeF :: Show a => Show (NodeF a) where
  show (Node x) = "(NodeF " <> show x <> ")"

--------------------------------------------------------------------------------

data MultiEdgeF f l s t = MultiEdge (Maybe l) (f (NodeF s)) (f (NodeF t))

instance showMultiEdgeF :: (Show l, Show s, Show t) => Show (MultiEdgeF List l s t) where
  show (MultiEdge l s t) = "(MultiEdge " <> show l <> " " <> show s <> " " <> show t <> ")"

derive instance eqMultiEdgeF :: (Eq l, Eq s, Eq t) => Eq (MultiEdgeF List l s t)

--------------------------------------------------------------------------------

-- | Generic graph element type.
data GElem1F a = GNode1      (NodeF a)
               | GMultiEdge1 (MultiEdgeF List a a a)

derive instance eqGElem1 :: Eq a => Eq (GElem1F a)

instance showGElem1 :: Show a => Show (GElem1F a) where
  show (GNode1 x)      = "(GNode1 "      <> show x <> ")"
  show (GMultiEdge1 x) = "(GMultiEdge1 " <> show x <> ")"
