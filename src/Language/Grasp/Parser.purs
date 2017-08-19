module Language.Grasp.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.List (List(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Language.Grasp.AST (Node(..), Edge(..), Label)
import Language.Grasp.Parser.Util

node :: Parser String Node
node = Node <$> label

edge :: Parser String Edge
edge = do
  src  <- node
  _    <- spaces
  lbl  <- arrow
  _    <- spaces
  dest <- node
  pure $ Edge lbl src dest

arrow :: Parser String (Maybe Label)
arrow = Nothing <$                                              string "->"
    <|> Just    <$> (string "-" *> spaces *> label <* spaces <* string "->")

label :: Parser String Label
label = someOf $ isAlphaNum || (_ == '_')
