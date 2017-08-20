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

import Language.Grasp.AST (Node(..), Edge(..), Label, GElem1(..))
import Language.Grasp.Parser.Util

graph1 :: Parser String (List GElem1)
graph1 = (gElem1 `inside` hspaces) `sepEndBy` (char ';' <|> char '\n')

gElem1 :: Parser String GElem1
gElem1 = GEdge1 <$> try edge
     <|> GNode1 <$>     node

--------------------------------------------------------------------------------

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
