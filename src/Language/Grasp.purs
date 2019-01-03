module Language.Grasp
  ( compile
  , OutputFormat(..)
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either)
import Language.Grasp.Generator.GraphViz as GraphViz
import Language.Grasp.Generator.PlantUML as PlantUML
import Language.Grasp.Parser as Parser
import Text.Parsing.Parser (runParser, ParseError)

data OutputFormat
  = GraphVizDigraph
  | PlantUMLSequenceDiagram

compile :: OutputFormat -> String -> String
compile outputFormat graspSrc =
  either (\err -> "Error: " <> show err) (\res -> res) generatedCode
  where
    generatedCode :: Either ParseError String
    generatedCode = generateCode <$> graspAST <*> pure styler

    generateCode = case outputFormat of
      GraphVizDigraph         -> GraphViz.digraph
      PlantUMLSequenceDiagram -> PlantUML.sequenceDiagram

    graspAST = runParser graspSrc Parser.graph1

    -- TODO parse stylesheet
    styler key = Nothing
