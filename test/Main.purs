module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.List (many, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Language.Grasp.AST (Node(..), Edge(..), GElem1(..))
import Language.Grasp.Generator.GraphViz as GraphViz
import Language.Grasp.Parser as Parser
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (satisfy)
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

main :: _
main = run [consoleReporter] do
  describe "label parsers" do
    itParses "x"          Parser.label        $ "x"
    itParses "x:A"        Parser.labelAndType $ "x" /\ Just "A"
    itParses "x: A"       Parser.labelAndType $ "x" /\ Just "A"
    itParses "x : A"      Parser.labelAndType $ "x" /\ Just "A"

  describe "node parsers" do
    itParses "abc,def"    Parser.node $ Node "abc"

  describe "edge parsers" do
    itParses "x->y"       Parser.edge $ Edge Nothing    (Node "x") (Node "y")
    itParses "x -> y"     Parser.edge $ Edge Nothing    (Node "x") (Node "y")
    itParses "x-f->y"     Parser.edge $ Edge (Just "f") (Node "x") (Node "y")
    itParses "x -f-> y"   Parser.edge $ Edge (Just "f") (Node "x") (Node "y")
    itParses "x - f -> y" Parser.edge $ Edge (Just "f") (Node "x") (Node "y")

    itParses "x:A -> y:B"                Parser.edge $ Edge Nothing    (Node "x") (Node "y")
    itParses "x : A -> y : B"            Parser.edge $ Edge Nothing    (Node "x") (Node "y")
    itParses "x:A -f:AtoB-> y:B"         Parser.edge $ Edge (Just "f") (Node "x") (Node "y")
    itParses "x : A - f : AtoB -> y : B" Parser.edge $ Edge (Just "f") (Node "x") (Node "y")

  describe "graph parsers" do
    itParses "x;x->y;y->z"    Parser.graph1 $ fromFoldable [GNode1 $ Node "x", GEdge1 $ Edge Nothing (Node "x") (Node "y"), GEdge1 $ Edge Nothing (Node "y") (Node "z")]
    itParses "x\nx->y\ny->z"  Parser.graph1 $ fromFoldable [GNode1 $ Node "x", GEdge1 $ Edge Nothing (Node "x") (Node "y"), GEdge1 $ Edge Nothing (Node "y") (Node "z")]
    itParses "x; x->y ; y->z" Parser.graph1 $ fromFoldable [GNode1 $ Node "x", GEdge1 $ Edge Nothing (Node "x") (Node "y"), GEdge1 $ Edge Nothing (Node "y") (Node "z")]

  describe "GraphViz backend" do
    it "should produce correct GraphViz output for simple graph" $ GraphViz.digraph
      [ GNode1 $ Node "x"
      , GEdge1 $ Edge Nothing (Node "x") (Node "y")
      , GEdge1 $ Edge Nothing (Node "y") (Node "z")
      ]
      (const Nothing)
      `shouldEqual`
      "digraph {\n  \"x\"\n  \"x\"->\"y\"\n  \"y\"->\"z\"\n}"

    it "should produce correct GraphViz output for styled graph" $ GraphViz.digraph
      [ GNode1 $ Node "x"
      , GEdge1 $ Edge Nothing (Node "x") (Node "y")
      , GEdge1 $ Edge Nothing (Node "y") (Node "z")
      ]
      (styleEnv [Tuple "x" {color: "red"}])
      `shouldEqual`
      "digraph {\n  \"x\" [color=\"red\"]\n  \"x\"->\"y\"\n  \"y\"->\"z\"\n}"

styleEnv = flip Map.lookup <<< Map.fromFoldable

itParses str p exp = it ("should parse: " <> str) $ (runParser str p) `shouldParseTo` exp

shouldParseTo v exp = shouldEqual v (Right exp)
