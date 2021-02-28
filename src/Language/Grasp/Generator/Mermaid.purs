module Language.Grasp.Generator.Mermaid where

import Prelude (class Functor, append, identity, map, mempty, pure, ($), (<$>), (<<<), (<>), (=<<))
import Data.List (List)
import Data.Foldable (class Foldable, intercalate, foldMap)
import Data.Maybe (Maybe, maybe)
import Data.String.Regex (replace, test)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple.Nested (type (/\),(/\))
import Language.Grasp.AST (GElem1, GElem1F(..), HyperEdge, HyperEdgeF(..), LabelAndType, Node, NodeF(..), Type, unNode)
import Language.Grasp.Generator (Styler, quote)
import Language.Grasp.Stylesheet.AST (SelectorElem(..))
import Language.Grasp.Stylesheet.AST as Stylesheet

graphDiagram :: forall f. Functor f => Foldable f => f GElem1 -> Styler -> String
graphDiagram g styler =
  "graph TD; \n  " <> intercalate "\n  " (fmtGElem1 styler <$> g) <> "\n"

fmtGElem1 :: Styler -> GElem1 -> String
fmtGElem1 styler (GNode1 n)      = fmtNode styler n
fmtGElem1 styler (GHyperEdge1 e) = fmtHyperEdge styler e

fmtNode :: Styler -> Node -> String
fmtNode styler (Node (label /\ typ)) =
  label <> "\n" <>
  "  style " <> label <> " " <> "fill: #dddd00"
  where
    style = foldMap (append " " <<< fmtNodeStyle) (styler (pure $ SNode label))

fmtHyperEdge :: Styler -> HyperEdge -> String
fmtHyperEdge styler (HyperEdge lMaybe srcNodes targetNodes) =
     (intercalate "," $ lt' <$> srcNodes)
  <> "-->"
  <> foldMap (\lt -> "|" <> (quote $ fmtLabelAndType lt) <> "|" <> " ") lMaybe
  <> (intercalate "," $ lt' <$> targetNodes)
  where
    lt' = replaceParentheses <<< fmtLabelAndType <<< unNode

fmtLabelAndType :: LabelAndType -> String
fmtLabelAndType (l /\ tm) = l <> fmtTypeAnno tm

fmtTypeAnno :: Maybe Type -> String
fmtTypeAnno = foldMap (":" <> _)

fmtNodeStyle :: Stylesheet.Attrs -> String
fmtNodeStyle attrs =
  "[" <> (intercalate "; " <<< map fmtAttr $ fromAttr =<< attrs) <> "]"
  where
    fmtAttr (k /\ v) = k <> "=" <> quote v

type MermaidAttr = Stylesheet.Key /\ String

replaceParentheses :: String -> String
replaceParentheses s = (replace pRegex "" s) <> label
  where
    label = if test pRegex s then "(\"" <> s <> "\")" else ""
    pRegex = unsafeRegex "[()]" global

fromAttr :: Stylesheet.Attr -> List MermaidAttr
fromAttr (k /\ v) = case k, v of
  "background", c -> (pure $ "fill"    /\ fromColor c)
                  <> (pure $ "stroke"  /\ fromColor c)
  _           , s -> mempty
  where
    fromColor = identity
