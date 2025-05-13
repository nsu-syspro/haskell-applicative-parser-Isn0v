{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import ParserCombinators
import Data.Char
import Data.List (intercalate)
import Control.Applicative
import Data.Functor
import Control.Monad (replicateM)
-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = whiteSpace *> choice [jsonObject, jsonArray, jsonString, jsonNumber, jsonBool, jsonNull] <* whiteSpace

whiteSpace :: Parser String
whiteSpace = many (choice (char <$> [' ', '\n', '\r', '\t']))

jsonNull :: Parser JValue
jsonNull = string "null" $> JNull

jsonBool :: Parser JValue
jsonBool = (string "true" $> JBool True) <|> (string "false" $> JBool False)


option :: b -> Parser b -> Parser b
option defaultValue parser = parser <|> pure defaultValue

jsonNumber :: Parser JValue
jsonNumber = JNumber . read <$> (option "" (string "-")
                        <> (string "0" <|> (((: []) <$> digitNoZero) <> many digit)))
                        <> option "" (string "." <> some digit)
                        <> option "" (choice [string "E", string "e"] <> option "" (choice [string "-", string "+"]) <> some digit)

digitNoZero :: Parser Char
digitNoZero = satisfy (\x -> '0' < x && x <= '9')

digit :: Parser Char
digit = satisfy isDigit


oneOf :: String -> Parser Char
oneOf = foldr ((<|>) . char) empty

jsonString :: Parser JValue
jsonString = JString <$> someString

someString :: Parser String
someString = char '"' *> stringContent <* char '"' where
  stringContent :: Parser String
  stringContent = concat <$> many (((: []) <$> satisfy (\x -> x /= '"' && x /= '\\')) <|> escapeChar)
  escapeChar = string "\\" <> (choice (string <$> ["\"", "\\", ['/'], "b", "f", "n", "r", "t"]) <|> replicateM 4 (satisfy isHexDigit))


jsonArray :: Parser JValue
jsonArray = JArray <$> (char '[' *> whiteSpace *> elements <* whiteSpace <* char ']')
  where elements = json `sepBy` (whiteSpace *> char ',' <* whiteSpace)

jsonObject :: Parser JValue
jsonObject = JObject <$> (char '{' *> whiteSpace *> pairs <* char '}')
  where
    pairs = sepBy pair (char ',' <* whiteSpace)
    pair = (,) <$> someString <* whiteSpace <* char ':' <* whiteSpace <*> json


sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep =
  (:) <$> p <*> many (sep *> p) <|> pure []

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
