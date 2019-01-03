module Language.Grasp.Stylesheet.AST where

import Data.Tuple (Tuple(..))

type Key = String

type Val = String

type Attr = Tuple Key Val

type Selector = String
