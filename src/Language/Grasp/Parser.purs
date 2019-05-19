module Language.Grasp.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Language.Grasp.AST (Node(..), Edge(..), MultiEdge(..), Label, Type, LabelAndType, GElem1(..))
import Language.Grasp.Parser.Util

import Debug.Trace (spy)

graph1 :: Parser String (List GElem1)
graph1 = (gElem1 `inside` hspaces) `sepEndBy` (semicolon <|> newlines)
  where
    newlines = skipMany1 (char '\n')
    semicolon = const unit <$> char ';'

gElem1 :: Parser String GElem1
gElem1 = GMultiEdge1 <$> try multiEdge
     <|> GNode1      <$>     node

node :: Parser String Node
node = Node <$> labelAndType

nodes :: Parser String (List Node)
nodes = (node `inside` hspaces) `sepEndBy1` char ','

multiEdge :: Parser String MultiEdge
multiEdge = do
  src  <- nodes
  _    <- hspaces
  lbl  <- arrow
  _    <- hspaces
  dest <- nodes
  pure $ MultiEdge lbl src dest

--------------------------------------------------------------------------------

arrow :: Parser String (Maybe LabelAndType)
arrow = Nothing <$                                                  string "->"
    <|> Just    <$> (string "-" *> labelAndType `inside` hspaces <* string "->")

labelAndType :: Parser String LabelAndType
labelAndType =
  Tuple <$> label <*> (try typeAscription <|> nothing)
  where
    typeAscription = Just <$> (colon *> ident)
    colon = (string ":") `inside` hspaces
    nothing = pure Nothing

-- TODO stick the argument(s) in a separate field
label :: Parser String Label
label = fmt <$> ident <*> argsM
  where
    fmt f x = f <> x
    fmtArgs x = "(" <> x <> ")"
    args = string "(" *> ident <* string ")"
    argsM = maybe "" fmtArgs <$> optionMaybe args

ident = someOf $ isAlphaNum || (_ == '_')
