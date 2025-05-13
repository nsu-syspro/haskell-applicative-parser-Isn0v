{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative
import Data.List (nub)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
  deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
  deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
  deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse (Parser p) input = p (Position 0 input)

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe parser input = case parse parser input of
  Parsed a _ -> Just a
  Failed _   -> Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> case p input of
    Parsed a rest -> Parsed (f a) rest
    Failed err    -> Failed err

instance Applicative Parser where
  pure a = Parser $ \input -> Parsed a input
  Parser pf <*> Parser pa = Parser $ \input -> case pf input of
    Parsed f rest -> case pa rest of
      Parsed a rest' -> Parsed (f a) rest'
      Failed err     -> Failed err
    Failed err -> Failed err

instance Alternative Parser where
  empty = Parser $ \_ -> Failed []
  Parser p1 <|> Parser p2 = Parser $ \input -> case p1 input of
    Parsed a rest -> Parsed a rest
    Failed err1   -> case p2 input of
      Parsed a rest -> Parsed a rest
      Failed err2   -> Failed (nub $ err1 ++ err2)


instance Monad Parser where 
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser (\input -> 
    case p input of
      Parsed value rest -> runParser (f value) rest
      Failed errors     -> Failed errors)

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \(Position pos input) -> case input of
  (x:xs) | predicate x -> Parsed x (Position (pos + 1) xs)
         | otherwise -> Failed [Position pos (Unexpected x)]
  [] -> Failed [Position pos EndOfInput]