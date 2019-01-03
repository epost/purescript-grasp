module Language.Grasp.Stylesheet.Parser where

import Prelude
import Data.List (List(..))
import Data.Tuple.Nested (type (/\), (/\))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Language.Grasp.Parser.Util
import Language.Grasp.Stylesheet.AST (Selector, Attr, Key, Val)

selectorWithAttrs :: Parser String (Selector /\ List Attr)
selectorWithAttrs = do
  sel <- selector
  _   <- string "{" `inside` spaces
  as  <- attrs
  _   <- string "}" `inside` spaces
  pure (sel /\ as)

attrs :: Parser String (List Attr)
attrs = (attr `inside` spaces) `sepBy` (semicolon `inside` spaces)

attr :: Parser String (Key /\ Val)
attr = do
  k <- key
  _ <- colon
  v <- val
  pure (k /\ v)
  where
    colon = string ":" `inside` hspaces

key :: Parser String Key
key = word

val :: Parser String Val
val = word

selector :: Parser String Selector
selector = word

word = someOf $ isAlphaNum || (_ == '_')

-- TODO dedupe
semicolon = const unit <$> char ';'
