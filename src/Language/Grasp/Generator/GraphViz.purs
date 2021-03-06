module Language.Grasp.Generator.GraphViz where

import Prelude
import Data.List as List
import Data.List (List)
import Data.Foldable (class Foldable, intercalate, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST
import Language.Grasp.AST.Util (edgeSourceAndTargetNodesSmashedTogether)
import Language.Grasp.Generator
import Language.Grasp.Stylesheet.AST as Stylesheet
import Language.Grasp.Stylesheet.AST (SelectorElem(..))

digraph :: forall f. List GElem1 -> Styler -> String
digraph g styler =
  "digraph {\n" <>
  "  // nodes to be styled\n" <>
  "  " <> intercalate "\n  " edgeSourceAndTargetNodesStyled <> "\n" <>
  "\n" <>
  "  // arrows\n" <>
  "  " <> intercalate "\n  " (fmtGElem1 styler <$> g) <> "\n" <>
  "}"
  where
    -- If we want styling to be applied to nodes that only occur as the source or target of some edge(s),
    -- we must make sure they are listed explicitly, so that styling can and will be applied to them.
    edgeSourceAndTargetNodesStyled = fmtNode styler <$> edgeSourceAndTargetNodesToStyle

    -- TODO Drop nodes that already occur as GNode1, as well as elements that have no style.
    edgeSourceAndTargetNodesToStyle :: List Node
    edgeSourceAndTargetNodesToStyle = List.nub <<< join <<< edgeSourceAndTargetNodesSmashedTogether $ g

fmtGElem1 :: Styler -> GElem1 -> String
fmtGElem1 styler (GNode1 n)      = fmtNode styler n
fmtGElem1 styler (GHyperEdge1 e) = fmtHyperEdge styler e

fmtNode :: Styler -> Node -> String
fmtNode styler (Node (label /\ typ)) =
  quote label <> style
  where
    style = foldMap (append " " <<< fmtNodeStyle) (styler (pure $ SNode label))

fmtHyperEdge :: Styler -> HyperEdge -> String
fmtHyperEdge styler (HyperEdge lMaybe srcNodes targetNodes) =
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
