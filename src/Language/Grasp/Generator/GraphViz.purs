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
digraph g styler = "digraph {\n  " <> (intercalate "\n  " (fmtGElem1 styler <$> g)) <> "\n}"

fmtGElem1 :: NodeStyler -> GElem1 -> String
fmtGElem1 styler (GNode1 n) = fmtNode styler n
fmtGElem1 styler (GEdge1 e) = fmtEdge styler e

fmtNode :: NodeStyler -> Node -> String
fmtNode styler (Node (label /\ typ)) =
  quote label <> style
  where
    style = foldMap (append " " <<< fmtNodeStyle) (styler label)

fmtEdge :: NodeStyler -> Edge -> String
fmtEdge styler (Edge lMaybe (Node lt1) (Node (lt2))) =
     fmtLabelAndType lt1
  <> "->"
  <> fmtLabelAndType lt2
  <> foldMap (\lt -> " [label=" <> fmtLabelAndType lt <> "]") lMaybe

fmtLabelAndType :: LabelAndType -> String
fmtLabelAndType (l /\ tm) = quote $ l <> fmtTypeAnno tm

fmtTypeAnno :: Maybe Type -> String
fmtTypeAnno = foldMap (": " <> _)

fmtNodeStyle :: NodeStyleRec -> String
fmtNodeStyle l = "["
  <> "color=" <> quote l.color
  <> "]"

quote :: String -> String
quote s = "\"" <> s <> "\""
