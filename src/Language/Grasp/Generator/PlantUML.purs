module Language.Grasp.Generator.PlantUML where

import Prelude
import Data.Array (filter)
import Data.Foldable (class Foldable, intercalate, foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested (type (/\),(/\))
import Language.Grasp.AST
import Language.Grasp.Generator
import Language.Grasp.Stylesheet.AST (SelectorElem(..))
import Language.Grasp.Stylesheet.AST as Stylesheet

sequenceDiagram :: forall f. Functor f => Foldable f => f GElem1 -> Styler -> String
sequenceDiagram g styler =
  "@startuml\n" <> foldMap (maybeNewline <<< fmtGElem1 styler) g <> "@enduml"
  where
    maybeNewline = maybe "" (_ <> "\n")

fmtGElem1 :: Styler -> GElem1 -> Maybe String
fmtGElem1 styler (GNode1 n)      =       fmtNode styler n
fmtGElem1 styler (GHyperEdge1 e) = Just "TODO HyperEdge case" -- Just (fmtEdge styler e)

fmtNode :: Styler -> Node -> Maybe String
fmtNode styler (Node (label /\ typ)) =
  case typ of
    Just "Actor"       -> Just $ "actor "       <> quote label <> style
    Just "Boundary"    -> Just $ "boundary "    <> quote label <> style
    Just "Control"     -> Just $ "control "     <> quote label <> style
    Just "Entity"      -> Just $ "entity "      <> quote label <> style
    Just "Database"    -> Just $ "database "    <> quote label <> style
    Just "Collections" -> Just $ "collections " <> quote label <> style
    Just _             -> Nothing
    Nothing            -> Nothing
  where
    style = foldMap fmtNodeStyle (styler (pure $ SNode label))

fmtHyperEdge :: Styler -> HyperEdge -> String
fmtHyperEdge styler (HyperEdge lMaybe srcNodes targetNodes) =
  intercalate "\n" (fmtEdge styler lMaybe <$> srcNodes <*> targetNodes)

fmtEdge :: Styler -> Maybe LabelAndType -> Node -> Node -> String
fmtEdge styler lMaybe (Node (label1 /\ _)) (Node (label2 /\ _)) =
  quote label1 <> " -> " <> quote label2 <> maybe "" (\(l /\ t) -> ": " <> l <> fmtType t) lMaybe
  where
    fmtType = maybe "" (\t -> ": " <> t)

fmtNodeStyle :: Stylesheet.Attrs -> String
fmtNodeStyle l = ""
