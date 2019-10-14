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

data MultiEdge = MultiEdge (Maybe LabelAndType) (List Node) (List Node)

derive instance nodeEq :: Eq Node

instance nodeShow :: Show Node where
  show (Node (label /\ typ)) = "(Node " <> label <> (foldMap (append ":") typ) <> ")"

instance multiEdgeShow :: Show MultiEdge where
  show (MultiEdge l s t) = "(MultiEdge " <> show l <> " " <> show s <> " " <> show t <> ")"

derive instance multiEdgeEq :: Eq MultiEdge

unNode :: Node -> LabelAndType
unNode (Node x) = x

--------------------------------------------------------------------------------

-- | A graph can be be represented by `List GElem1`.
data GElem1 = GNode1 Node
            | GMultiEdge1 MultiEdge

derive instance gelem1Eq :: Eq GElem1

instance gelem1Show :: Show GElem1 where
  show (GNode1 x)      = "(GNode1 "      <> show x <> ")"
  show (GMultiEdge1 x) = "(GMultiEdge1 " <> show x <> ")"
