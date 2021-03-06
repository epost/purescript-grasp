module Language.Grasp
  ( compile
  , compileWithStylesheet
  , OutputFormat(..)
  ) where

import Prelude
import Data.Map as Map
import Data.Either (Either(..), either)
import Language.Grasp.Generator.GraphViz as GraphViz
import Language.Grasp.Generator.PlantUML as PlantUML
import Language.Grasp.Generator.HyperGraphJson as HyperGraphJson
import Language.Grasp.Generator.HyperGraphGraphViz as HyperGraphGraphViz
import Language.Grasp.Parser as Parser
import Language.Grasp.Stylesheet.Parser as SSParser
import Language.Grasp.Stylesheet.AST (Stylesheet)
import Text.Parsing.Parser (runParser, ParseError)

import Debug.Trace

data OutputFormat
  = GraphVizDigraph
  | PlantUMLSequenceDiagram
  | HyperGraphJson
  | HyperGraphGraphViz

compile :: OutputFormat -> String -> String
compile outputFormat graspSrc =
  compileWithStylesheet outputFormat graspSrc ""

compileWithStylesheet :: OutputFormat -> String -> String -> String
compileWithStylesheet outputFormat graspSrc stylesheetSrc =
  either (\err -> "Error: " <> show err) (\res -> res) generatedCode
  where
    generatedCode :: Either ParseError String
    generatedCode = generateCode <$> graspAST <*> pure styler

    generateCode = case outputFormat of
      GraphVizDigraph         -> GraphViz.digraph
      PlantUMLSequenceDiagram -> PlantUML.sequenceDiagram
      HyperGraphJson          -> flip \_ -> HyperGraphJson.hyperGraphJsonString
      HyperGraphGraphViz      -> HyperGraphGraphViz.digraph

    graspAST = runParser graspSrc Parser.graph1

    styler key = Map.lookup key stylesheet

    stylesheet = either (const defaultStylesheet) Map.fromFoldable $ compileStylesheet stylesheetSrc

    -- TODO don't fail silently in case of stylesheet parse error
    defaultStylesheet = mempty

compileStylesheet :: String -> Either ParseError Stylesheet
compileStylesheet ssText =
  runParser ssText SSParser.stylesheet
