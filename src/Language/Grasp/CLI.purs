-- | Functions for invoking Grasp via JavaScript and the command line.
module Language.Grasp.CLI
  ( compile
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either)
import Language.Grasp as Grasp
import Language.Grasp (OutputFormat(..))
import Language.Grasp.Generator.GraphViz as GraphViz
import Language.Grasp.Generator.PlantUML as PlantUML
import Language.Grasp.Parser as Parser
import Text.Parsing.Parser (runParser, ParseError)

compile :: String -> String -> String -> String
compile outputFormatOpt graspSrc stylesheetSrc  =
  Grasp.compileWithStylesheet outputFormat graspSrc stylesheetSrc
  where
    defaultOutputFormat = GraphVizDigraph
    outputFormat = fromMaybe defaultOutputFormat $ parseOutputFormatOpt outputFormatOpt

parseOutputFormatOpt :: String -> Maybe OutputFormat
parseOutputFormatOpt = case _ of
  "-g" -> Just $ GraphVizDigraph
  "-p" -> Just $ PlantUMLSequenceDiagram
  _    -> Nothing
