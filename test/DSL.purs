-- |  Some very rough and experimental bits of DSL to facilitate graph construction.
module Test.Language.Grasp.DSL where

import Prelude
import Data.Either (Either(..), either)
import Data.List (many, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST (Node(..), MultiEdge(..), GElem1(..), Label, Type, LabelAndType)

-- | Construct an untyped list of nodes.
node  l =        Node (l : Nothing)
nodes l = pure $ Node (l : Nothing)

oftype :: Label -> Maybe Type -> LabelAndType
oftype l t = l /\ t
infixl 1 oftype as :

typepatch ns t' = map (\(Node (l /\ t)) -> Node (l /\ t')) ns
infixl 1 typepatch as :::

n1 =            node
n2 = GNode1 <<< node
n  = n2

nt :: String -> String -> GElem1
nt l t = GNode1 $ Node (l : Just t)

to1        =               MultiEdge Nothing
to2 x y    =               MultiEdge Nothing   (nodes x) (nodes y)
to3 x y    = GMultiEdge1 $ MultiEdge Nothing   (nodes x) (nodes y)
to4 x y lt = GMultiEdge1 $ MultiEdge (Just lt) (nodes x) (nodes y)

infixl 4 to1 as ~>
infixl 4 to2 as ~~>
infixl 4 to3 as ~~~>
