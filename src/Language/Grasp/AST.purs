module Language.Grasp.AST where

import Prelude
import Data.Eq (class Eq1)
import Data.List (List(..))
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldMap)

type Label = String

-- | This captures labels with (type) annotations, which may look something like `x:a`.
type LabelAndType = Label /\ Maybe Type

type Type = String

type Node = NodeF LabelAndType

type HyperEdge = HyperEdgeF List LabelAndType

-- | Graph element type. A graph is a bunch of nodes and edges, and can be
-- | represented as a `List GElem1` for example.
type GElem1 = GElem1F List LabelAndType

--------------------------------------------------------------------------------

newtype NodeF a = Node a

unNode :: forall a. NodeF a -> a
unNode (Node x) = x

derive instance eqNodeF :: Eq a => Eq (NodeF a)

instance showNodeF :: Show a => Show (NodeF a) where
  show (Node x) = "(NodeF " <> show x <> ")"

derive instance functorNodeF :: Functor NodeF

--------------------------------------------------------------------------------

-- | A hyperedge, possibly labeled with type a, going from a's to a's.
data HyperEdgeF f a = HyperEdge (Maybe a) (f (NodeF a)) (f (NodeF a))

instance showHyperEdgeF :: (Show a, Show (f (NodeF a))) => Show (HyperEdgeF f a) where
  show (HyperEdge l s t) = "(HyperEdge " <> show l <> " " <> show s <> " " <> show t <> ")"

derive instance eqHyperEdgeF :: (Eq1 f, Eq a) => Eq (HyperEdgeF f a)

derive instance functorHyperEdgeF :: Functor f => Functor (HyperEdgeF f)

--------------------------------------------------------------------------------

-- | Generic graph element type.
data GElem1F f a = GNode1      (NodeF a)
                 | GHyperEdge1 (HyperEdgeF f a)

derive instance eqGElem1F :: (Eq a, Eq (HyperEdgeF f a)) => Eq (GElem1F f a)

derive instance functorGElem1F :: Functor f => Functor (GElem1F f)

instance showGElem1 :: (Show a, Show (HyperEdgeF f a)) => Show (GElem1F f a) where
  show (GNode1 x)      = "(GNode1 "      <> show x <> ")"
  show (GHyperEdge1 x) = "(GHyperEdge1 " <> show x <> ")"
