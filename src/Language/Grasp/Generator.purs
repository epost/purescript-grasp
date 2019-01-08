module Language.Grasp.Generator where

import Prelude ((<>))
import Language.Grasp.AST
import Language.Grasp.Stylesheet.AST as Stylesheet
import Language.Grasp.Stylesheet.AST (Selector, SelectorElem(..))
import Data.Maybe (Maybe(..))

type Styler = Selector -> Maybe Stylesheet.Attrs

quote :: String -> String
quote s = "\"" <> s <> "\""
