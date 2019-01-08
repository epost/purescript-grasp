module Language.Grasp
  ( compile
  , compileWithStylesheet
  , OutputFormat(..)
  ) where

import Prelude
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either)
import Language.Grasp.Generator.GraphViz as GraphViz
import Language.Grasp.Generator.PlantUML as PlantUML
import Language.Grasp.Parser as Parser
import Language.Grasp.Stylesheet.Parser as SSParser
import Text.Parsing.Parser (runParser, ParseError)

data OutputFormat
  = GraphVizDigraph
  | PlantUMLSequenceDiagram

compile :: OutputFormat -> String -> String
compile outputFormat graspSrc =
  compileWithStylesheet outputFormat graspSrc ""

compileWithStylesheet :: OutputFormat -> String -> String -> String
compileWithStylesheet  outputFormat graspSrc stylesheetSrc =

  either (\err -> "Error: " <> show err) (\res -> res) generatedCode
  where
    generatedCode :: Either ParseError String
    generatedCode = generateCode <$> graspAST <*> pure styler

    generateCode = case outputFormat of
      GraphVizDigraph         -> GraphViz.digraph
      PlantUMLSequenceDiagram -> PlantUML.sequenceDiagram

    graspAST = runParser graspSrc Parser.graph1

    styler key = Map.lookup key stylesheet

    stylesheet = either (const defaultStylesheet) Map.fromFoldable $ compileStylesheet stylesheetSrc

    -- TODO don't fail silently in case of stylesheet parse error
    defaultStylesheet = mempty

compileStylesheet :: String -> _
compileStylesheet ssText =
  runParser ssText SSParser.stylesheet
