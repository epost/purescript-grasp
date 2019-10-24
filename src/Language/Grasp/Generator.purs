module Language.Grasp.Generator where

import Prelude ((<>))
import Language.Grasp.Stylesheet.AST as Stylesheet
import Language.Grasp.Stylesheet.AST (Selector)
import Data.Maybe (Maybe(..))

type Styler = Selector -> Maybe Stylesheet.Attrs

quote :: String -> String
quote s = "\"" <> s <> "\""
