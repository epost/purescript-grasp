module Test.Language.Grasp.Stylesheet where

import Prelude
import Data.Either (Either(..))
import Data.List as List
import Data.Tuple.Nested ((/\))
import Language.Grasp.Parser as Parser
import Language.Grasp.Stylesheet.Parser as Parser
import Text.Parsing.Parser  (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

spec = do
  describe "grasp stylesheet parser" do
    itParses "x {\n shape:box;\n color : red \n }\n" Parser.selectorWithAttrs $
      "x" /\ List.fromFoldable
             [ "shape" /\ "box"
             , "color" /\ "red"
             ]

-- TODO dedupe
itParses str p exp = it ("should parse: \"" <> str <> "\" as " <> show exp) $ (runParser str p) `shouldParseTo` exp

-- TODO dedupe
shouldParseTo v exp = shouldEqual v (Right exp)
