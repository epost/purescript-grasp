module Language.Grasp.Parser.Util where

import Prelude
import Data.Array (some, many)
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.String as String
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (skipMany, optional)
import Text.Parsing.Parser.String (satisfy)

-- adapted from https://github.com/slamdata/purescript-markdown/blob/master/src/Text/Markdown/SlamDown/Parser/Inline.purs

isAlphaNum :: Char -> Boolean
isAlphaNum = isAlpha || isDigit

isAlpha :: Char -> Boolean
isAlpha = isAlphaLower || isAlphaUpper

isAlphaLower :: Char -> Boolean
isAlphaLower c = c >= 'a' && c <= 'z'

isAlphaUpper :: Char -> Boolean
isAlphaUpper c = c >= 'A' && c <= 'Z'

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isWhitespace :: Char -> Boolean
isWhitespace = R.test wsRegex <<< singleton
  where
    wsRegex = unsafeRegex "^\\s$" noFlags
    flags = { unicode: false
            , sticky: false
            , multiline: false
            , ignoreCase: false
            , global: false
            }

someOf :: (Char -> Boolean) -> Parser String String
someOf p = fromCharArray <$> some (satisfy p)

manyOf :: (Char -> Boolean) -> Parser String String
manyOf p = fromCharArray <$> many (satisfy p)

inside :: forall a b. Parser String a -> Parser String b -> Parser String a
inside x ignore = ignore *> x <* ignore

spaces :: Parser String Unit
spaces = skipMany (satisfy isWhitespace)

hspaces :: Parser String Unit
hspaces = skipMany (satisfy (isWhitespace && (_ /= '\n')))
