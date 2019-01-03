module Language.Grasp
  ( compileCLI
  , compile
  , OutputFormat(..)
  , toGraphViz
  , toPlantUML
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

-- CLI -------------------------------------------------------------------------

compileCLI :: String -> String -> String
compileCLI outputFormatOpt graspSrc =
  compile outputFormat graspSrc
  where
    defaultOutputFormat = GraphVizDigraph
    outputFormat = fromMaybe defaultOutputFormat $ parseOutputFormatOpt outputFormatOpt

parseOutputFormatOpt :: String -> Maybe OutputFormat
parseOutputFormatOpt = case _ of
  "-g" -> Just $ GraphVizDigraph
  "-p" -> Just $ PlantUMLSequenceDiagram
  _    -> Nothing

toGraphViz :: String -> String
toGraphViz graspSrc = compile GraphVizDigraph graspSrc

toPlantUML :: String -> String
toPlantUML graspSrc = compile PlantUMLSequenceDiagram graspSrc
