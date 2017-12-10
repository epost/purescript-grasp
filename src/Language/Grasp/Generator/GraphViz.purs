module Language.Grasp.Generator.GraphViz where

import Prelude
import Data.Foldable (class Foldable, intercalate, foldMap)
import Data.Map as Map
import Data.Map (Map(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST
import Language.Grasp.Generator

digraph :: forall f. Functor f => Foldable f => f GElem1 -> NodeStyler -> String
digraph g styler = "digraph {\n  " <> (intercalate "\n" (fmtGElem1 styler <$> g)) <> "\n}"

fmtGElem1 :: NodeStyler -> GElem1 -> String
fmtGElem1 styler (GNode1 n) = fmtNode styler n
fmtGElem1 styler (GEdge1 e) = fmtEdge styler e

fmtNode :: NodeStyler -> Node -> String
fmtNode styler (Node (label /\ typ)) = quote label <> style
  where
    style = foldMap (append " " <<< fmtNodeStyle) (styler label)

fmtEdge :: NodeStyler -> Edge -> String
fmtEdge styler (Edge lMaybe (Node (label1 /\ _)) (Node (label2 /\ _))) =
  "  " <> quote label1 <> "->" <> quote label2 <> maybe "" (\(l /\ t) -> "[label=" <> quote l <> "]") lMaybe

fmtNodeStyle :: NodeStyleRec -> String
fmtNodeStyle l = "["
  <> "color=" <> quote l.color
  <> "]"

quote :: String -> String
quote s = "\"" <> s <> "\""
