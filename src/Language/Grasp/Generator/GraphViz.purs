module Language.Grasp.Generator.GraphViz where

import Prelude
import Data.List (catMaybes, mapMaybe, List)
import Data.Foldable (class Foldable, intercalate, foldMap)
import Data.Map as Map
import Data.Map (Map(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST
import Language.Grasp.Generator
import Language.Grasp.Stylesheet.AST as Stylesheet
import Language.Grasp.Stylesheet.AST (SelectorElem(..))

digraph :: forall f. Functor f => Foldable f => f GElem1 -> Styler -> String
digraph g styler = "digraph {\n  " <> (intercalate "\n  " (fmtGElem1 styler <$> g)) <> "\n}"

fmtGElem1 :: Styler -> GElem1 -> String
fmtGElem1 styler (GNode1 n)      = fmtNode styler n
fmtGElem1 styler (GMultiEdge1 e) = fmtMultiEdge styler e

fmtNode :: Styler -> Node -> String
fmtNode styler (Node (label /\ typ)) =
  quote label <> style
  where
    style = foldMap (append " " <<< fmtNodeStyle) (styler (pure $ SNode label))

fmtMultiEdge :: Styler -> MultiEdge -> String
fmtMultiEdge styler (MultiEdge lMaybe srcNodes targetNodes) =
     (intercalate "," $ (fmtLabelAndType <<< unNode) <$> srcNodes)
  <> "->"
  <> (intercalate "," $ (fmtLabelAndType <<< unNode) <$> targetNodes)
  <> foldMap (\lt -> " [label=" <> fmtLabelAndType lt <> "]") lMaybe

fmtLabelAndType :: LabelAndType -> String
fmtLabelAndType (l /\ tm) = quote $ l <> fmtTypeAnno tm

fmtTypeAnno :: Maybe Type -> String
fmtTypeAnno = foldMap (": " <> _)

fmtNodeStyle :: Stylesheet.Attrs -> String
fmtNodeStyle attrs =
  "[" <> (intercalate "; " <<< map fmtAttr $ fromAttr =<< attrs) <> "]"
  where
    fmtAttr (k /\ v) = k <> "=" <> quote v

quote :: String -> String
quote s = "\"" <> s <> "\""

--------------------------------------------------------------------------------

type GraphVizAttr = Stylesheet.Key /\ String

fromAttr :: Stylesheet.Attr -> List GraphVizAttr
fromAttr (k /\ v) = case k, v of
  "label"     , s -> (pure $ "label"     /\ s)
  "xlabel"    , s -> (pure $ "xlabel"    /\ s)
  "shape"     , s -> (pure $ "shape"     /\ fromShape s)
  "background", c -> (pure $ "fillcolor" /\ fromColor c)
                  <> (pure $ "style"     /\ "filled")
  _           , s -> mempty
  where
    fromColor = identity
    fromShape = identity
