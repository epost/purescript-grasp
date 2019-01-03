module Language.Grasp (toGraphViz) where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Maybe (Maybe(..))
import Data.Either (either)
import Language.Grasp.Generator.GraphViz as GraphViz
import Language.Grasp.Parser as Parser
import Text.Parsing.Parser (runParser)

toGraphViz :: String -> String
toGraphViz graspSrc = do
  let
    gE         = runParser graspSrc Parser.graph1
    styler key = Nothing
    gvE        = flip GraphViz.digraph styler <$> gE
  either (\err -> "Error: " <> show err) (\res -> res) gvE
