module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Either (Either(..))
import Data.List (many, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST (Node(..), Edge(..), GElem1(..), Label, Type, LabelAndType, NodeStyleRec)
import Language.Grasp.Generator.GraphViz as GraphViz
import Language.Grasp.Generator.PlantUML as PlantUML
import Language.Grasp.Parser as Parser
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (satisfy)
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Language.Grasp.DSL

main :: _
main = run [consoleReporter] do
  describe "label parsers" do
    itParses "x"          Parser.labelAndType $ "x" : Nothing
    itParses "x:A"        Parser.labelAndType $ "x" : Just "A"
    itParses "x: A"       Parser.labelAndType $ "x" : Just "A"
    itParses "x : A"      Parser.labelAndType $ "x" : Just "A"

  describe "node parsers" do
    itParses "abc,def"    Parser.node $ node "abc"

  describe "edge parsers" do
    itParses "x->y"       Parser.edge $ Edge Nothing                (node "x") (node "y")
    itParses "x -> y"     Parser.edge $ Edge Nothing                (node "x") (node "y")
    itParses "x-f->y"     Parser.edge $ Edge (Just ("f" : Nothing)) (node "x") (node "y")
    itParses "x -f-> y"   Parser.edge $ Edge (Just ("f" : Nothing)) (node "x") (node "y")
    itParses "x - f -> y" Parser.edge $ Edge (Just ("f" : Nothing)) (node "x") (node "y")

    itParses "x:A -> y:B"                Parser.edge $ Edge Nothing                    (node "x" ::: Just "A") (node "y" ::: Just "B")
    itParses "x : A -> y : B"            Parser.edge $ Edge Nothing                    (node "x" ::: Just "A") (node "y" ::: Just "B")
    itParses "x:A -f:AtoB-> y:B"         Parser.edge $ Edge (Just ("f" : Just "AtoB")) (node "x" ::: Just "A") (node "y" ::: Just "B")
    itParses "x : A - f : AtoB -> y : B" Parser.edge $ Edge (Just ("f" : Just "AtoB")) (node "x" ::: Just "A") (node "y" ::: Just "B")

  describe "graph parsers" do
    itParses "x;x->y;y->z"    Parser.graph1 $ fromFoldable [n "x", "x" ~~~> "y", "y" ~~~> "z"]
    itParses "x\nx->y\ny->z"  Parser.graph1 $ fromFoldable [n "x", "x" ~~~> "y", "y" ~~~> "z"]
    itParses "x; x->y ; y->z" Parser.graph1 $ fromFoldable [n "x", "x" ~~~> "y", "y" ~~~> "z"]

  describe "GraphViz backend" do
    it "should produce correct GraphViz output for simple graph" $
      GraphViz.digraph
      [n "x", "x" ~~~> "y", "y" ~~~> "z"]
      (const Nothing)
      `shouldEqual`
      "digraph {\n  \"x\"\n  \"x\"->\"y\"\n  \"y\"->\"z\"\n}"

    it "should produce correct GraphViz output for styled graph" $ GraphViz.digraph
      [n "x", "x" ~~~> "y", "y" ~~~> "z"]
      (styleEnv ["x" /\ {color: "red"}])
      `shouldEqual`
      "digraph {\n  \"x\" [color=\"red\"]\n  \"x\"->\"y\"\n  \"y\"->\"z\"\n}"

  describe "PlantUML backend" do
    it "should produce a correct PlantUML sequence diagram" do
      PlantUML.sequenceDiagram
        [ nt "user" "Actor"
        , to4 "user" "browser" ("login" : pure "Credentials")
        , "browser" ~~~> "z"
        ]
        (styleEnv ["user" /\ {color: "red"}])
        `shouldEqual`
        ("@startuml\n" <>
         "actor \"user\"\n" <>
         "\"user\" -> \"browser\": login: Credentials\n" <>
         "\"browser\" -> \"z\"\n@enduml")

styleEnv :: Array (String /\ NodeStyleRec) -> String -> Maybe NodeStyleRec
styleEnv = flip Map.lookup <<< Map.fromFoldable

itParses str p exp = it ("should parse: \"" <> str <> "\" as " <> show exp) $ (runParser str p) `shouldParseTo` exp

shouldParseTo v exp = shouldEqual v (Right exp)
