module Language.Grasp.Generator where

import Prelude ((<>))
import Language.Grasp.AST
import Data.Maybe (Maybe(..))

type NodeStyler = Label -> Maybe NodeStyleRec

quote :: String -> String
quote s = "\"" <> s <> "\""
