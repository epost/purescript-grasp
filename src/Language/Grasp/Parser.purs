module Language.Grasp.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Language.Grasp.AST (Node(..), Edge(..), Label, Type, LabelAndType, GElem1(..))
import Language.Grasp.Parser.Util

graph1 :: Parser String (List GElem1)
graph1 = (gElem1 `inside` hspaces) `sepEndBy` (char ';' <|> char '\n')

gElem1 :: Parser String GElem1
gElem1 = GEdge1 <$> try edge
     <|> GNode1 <$>     node

--------------------------------------------------------------------------------

node :: Parser String Node
node = Node <$> labelAndType

edge :: Parser String Edge
edge = do
  src  <- node
  _    <- hspaces
  lbl  <- arrow
  _    <- hspaces
  dest <- node
  pure $ Edge lbl src dest

arrow :: Parser String (Maybe LabelAndType)
arrow = Nothing <$                                                  string "->"
    <|> Just    <$> (string "-" *> labelAndType `inside` hspaces <* string "->")

labelAndType :: Parser String LabelAndType
labelAndType =
  Tuple <$> ident <*> (try typeAscription <|> nothing)
  where
    typeAscription = Just <$> (colon *> ident)
    colon = (string ":") `inside` hspaces
    nothing = pure Nothing

ident = someOf $ isAlphaNum || (_ == '_')
