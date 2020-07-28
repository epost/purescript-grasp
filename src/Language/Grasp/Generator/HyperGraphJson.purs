module Language.Grasp.Generator.HyperGraphJson where

import Prelude
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Core (Json(..))
import Data.Array as Array
import Data.Foldable (class Foldable, intercalate, foldMap)
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as Object
import Language.Grasp.AST
import Language.Grasp.AST.Optics (getNodeLabel)
import Language.Grasp.Generator
import Language.Grasp.Generator.HyperGraph (defaultEdgeName_HACK)


hyperGraphJsonString :: forall f. Functor f => Foldable f => f GElem1 -> String
hyperGraphJsonString = Argonaut.stringify <<< hyperGraphJson

hyperGraphJson :: forall f. Functor f => Foldable f => f GElem1 -> Json
hyperGraphJson = Argonaut.fromArray <<< Array.fromFoldable <<< map fromGElem1

fromGElem1 :: GElem1 -> Json
fromGElem1 node = case node of
  GNode1 n      -> Argonaut.fromString "TODO_fromGElem_node_case"
  GHyperEdge1 e -> fromHyperEdge e

fromHyperEdge :: HyperEdge -> Json
fromHyperEdge (HyperEdge lMaybe srcNodes targetNodes) =
  Argonaut.fromObject $ Object.fromFoldable
    [ "id" /\ (lMaybe      # Argonaut.fromString <<< maybe defaultEdgeName_HACK fst)
    , "s"  /\ (srcNodes    # Argonaut.fromArray <<< Array.fromFoldable <<< map (Argonaut.fromString <<< getNodeLabel))
    , "t"  /\ (targetNodes # Argonaut.fromArray <<< Array.fromFoldable <<< map (Argonaut.fromString <<< getNodeLabel))
    ]
