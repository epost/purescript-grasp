-- | Functions for invoking Grasp via JavaScript and the command line.
module Language.Grasp.CLI
  ( compile
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Language.Grasp as Grasp
import Language.Grasp (OutputFormat(..))

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
