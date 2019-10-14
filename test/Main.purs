module Test.Main where

import Prelude
import Effect.Aff (Aff, launchAff)
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Either (Either(..))
import Data.List as List
import Data.List (many, fromFoldable, List(..))
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Grasp.AST (Node(..), MultiEdge(..), GElem1(..), Label, Type, LabelAndType)
import Language.Grasp.Generator.GraphViz as GraphViz
import Language.Grasp.Generator.PlantUML as PlantUML
import Language.Grasp.Parser as Parser
import Language.Grasp.Stylesheet.AST as Stylesheet
import Language.Grasp.Stylesheet.AST (Selector, SelectorElem(..))
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (satisfy)
import Test.Spec                  (describe, pending, it)
import Test.Spec.Console          (write)
import Test.Spec.Runner           (runSpec)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Language.Grasp.DSL
import Test.Language.Grasp.Stylesheet as Stylesheet

main :: Effect _
main = launchAff $ runSpec [consoleReporter] do
  describe "label parsers" do
    itParses "x"          Parser.labelAndType $ "x" : Nothing
    itParses "x:A"        Parser.labelAndType $ "x" : Just "A"
    itParses "x: A"       Parser.labelAndType $ "x" : Just "A"
    itParses "x : A"      Parser.labelAndType $ "x" : Just "A"

  describe "node parsers" do
    itParses "abc,def"    Parser.node $ node "abc"

  describe "edge parsers" do
    itParses "x->y"                      Parser.multiEdge $ MultiEdge Nothing                    (nodes "x")              (nodes "y")
    itParses "x -> y"                    Parser.multiEdge $ MultiEdge Nothing                    (nodes "x")              (nodes "y")
    itParses "x-f->y"                    Parser.multiEdge $ MultiEdge (Just ("f" : Nothing))     (nodes "x")              (nodes "y")
    itParses "x -f-> y"                  Parser.multiEdge $ MultiEdge (Just ("f" : Nothing))     (nodes "x")              (nodes "y")
    itParses "x - f -> y"                Parser.multiEdge $ MultiEdge (Just ("f" : Nothing))     (nodes "x")              (nodes "y")

    itParses "x:A -> y:B"                Parser.multiEdge $ MultiEdge Nothing                    (nodes "x" ::: Just "A") (nodes "y" ::: Just "B")
    itParses "x : A -> y : B"            Parser.multiEdge $ MultiEdge Nothing                    (nodes "x" ::: Just "A") (nodes "y" ::: Just "B")
    itParses "x:A -f:AtoB-> y:B"         Parser.multiEdge $ MultiEdge (Just ("f" : Just "AtoB")) (nodes "x" ::: Just "A") (nodes "y" ::: Just "B")
    itParses "x : A - f : AtoB -> y : B" Parser.multiEdge $ MultiEdge (Just ("f" : Just "AtoB")) (nodes "x" ::: Just "A") (nodes "y" ::: Just "B")

    itParses "x,y->z"                    Parser.multiEdge $ MultiEdge Nothing      (Cons (node "x") (Cons (node "y") Nil))
                                                                                                    (Cons (node "z") Nil)
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
      (styleEnv [(pure $ SNode "x") /\ ["background" /\ "red"]])
      `shouldEqual`
      "digraph {\n  \"x\" [fillcolor=\"red\"; style=\"filled\"]\n  \"x\"->\"y\"\n  \"y\"->\"z\"\n}"

  describe "PlantUML backend" do
    it "should produce a correct PlantUML sequence diagram" do
      PlantUML.sequenceDiagram
        [ nt "user" "Actor"
        , to4 "user" "browser" ("login" : pure "Credentials")
        , "browser" ~~~> "z"
        ]
        (styleEnv [])
        `shouldEqual`
        ("@startuml\n" <>
         "actor \"user\"\n" <>
         "\"user\" -> \"browser\": login: Credentials\n" <>
         "\"browser\" -> \"z\"\n@enduml")

  Stylesheet.spec

styleEnv :: Array (Selector /\ Array Stylesheet.Attr) -> Selector -> Maybe Stylesheet.Attrs
styleEnv = flip Map.lookup <<< Map.fromFoldable <<< map (map List.fromFoldable)

itParses str p exp = it ("should parse: \"" <> str <> "\" as " <> show exp) $ (runParser str p) `shouldParseTo` exp

shouldParseTo v exp = shouldEqual v (Right exp)
