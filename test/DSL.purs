-- |  Some very rough and experimental bits of DSL to facilitate graph construction.
module Test.Language.Grasp.DSL where

import Prelude
import Data.Either (Either(..), either)
import Data.List (many, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST (Node(..), Edge(..), GElem1(..), Label, Type, LabelAndType)

-- | Construct an untyped node.
node l = Node (l : Nothing)

oftype :: Label -> Maybe Type -> LabelAndType
oftype l t = l /\ t
infixl 1 oftype as :

typepatch (Node (l /\ t)) t' = Node (l /\ t')
infixl 1 typepatch as :::

n1 =            node
n2 = GNode1 <<< node
n  = n2

nt :: String -> String -> GElem1
nt l t = GNode1 $ Node (l : Just t)

to1        =          Edge Nothing
to2 x y    =          Edge Nothing   (node x) (node y)
to3 x y    = GEdge1 $ Edge Nothing   (node x) (node y)
to4 x y lt = GEdge1 $ Edge (Just lt) (node x) (node y)

infixl 4 to1 as ~>
infixl 4 to2 as ~~>
infixl 4 to3 as ~~~>
