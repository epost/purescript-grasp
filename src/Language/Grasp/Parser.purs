module Language.Grasp.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.String as String
import Data.List (List(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Language.Grasp.AST (Node(..), Edge(..))
import Language.Grasp.Parser.Util

node :: Parser String Node
node = Node <$> label

edge :: Parser String Edge
edge = do
  src <- node
  _ <- inSpaces (string "->")
  dest <- node
  pure $ Edge src dest

label :: Parser String String
label = someOf $ isAlphaNum || (_ == '_')
