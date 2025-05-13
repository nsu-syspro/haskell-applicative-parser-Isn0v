{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators
import Task1
import Control.Applicative
import Control.Monad
import Data.Maybe

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

dayFormal :: Parser Day
dayFormal = Day <$> do
  d <- choice [string "0", string "1", string "2", string "3"]
  m <- choice [string "0", string "1", string "2", string "3", string "4", string "5", string "6", string "7", string "8", string "9"]
  let n = read (d ++ m) :: Int
  guard (n >= 1 && n <= 31)
  return n

dayInformal :: Parser Day
dayInformal = Day <$> do
  d <- choice [string "1", string "2", string "3", string "4", string "5", string "6", string "7", string "8", string "9"]
  m <- optional (choice [string "0", string "1", string "2", string "3", string "4", string "5", string "6", string "7", string "8", string "9"])
  let n = read (d ++ fromMaybe "" m) :: Int
  guard (n >= 1 && n <= 31)
  return n


monthFromInteger :: Parser Month
monthFromInteger = Month <$> do
  d <- choice [string "0", string "1"]
  m <- choice [string "0", string "1", string "2", string "3", string "4", string "5", string "6", string "7", string "8", string "9"]
  let n = read (d ++ m) :: Int
  guard (n >= 1 && n <= 12)
  return n

monthFromString :: Parser Month
monthFromString = Month <$> do
  m <- choice [string "Jan", string "Feb", string "Mar", string "Apr", string "May", string "Jun", string "Jul", string "Aug", string "Sep", string "Oct", string "Nov", string "Dec"]
  let n = case m of
        "Jan" -> 1
        "Feb" -> 2
        "Mar" -> 3
        "Apr" -> 4
        "May" -> 5
        "Jun" -> 6
        "Jul" -> 7
        "Aug" -> 8
        "Sep" -> 9
        "Oct" -> 10
        "Nov" -> 11
        "Dec" -> 12
        _     -> 0 -- get empty after guard
  guard (n >= 1 && n <= 12)
  return n

  
month :: Parser Month
month = monthFromInteger <|> monthFromString


year :: Parser Year
year = Year . fromInteger <$> nat



-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDate ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = dateWithDots
   <|> dateWithHyphens
   <|> dateWithSpaces


dateWithDots :: Parser Date
dateWithDots = Date <$> (dayFormal <* char '.') <*> (month <* char '.') <*> year

dateWithHyphens :: Parser Date
dateWithHyphens = Date <$> (dayFormal <* char '-') <*> (month <* char '-') <*> year

dateWithSpaces :: Parser Date
dateWithSpaces = flip Date <$> (month <* spaces) <*> (dayInformal <* spaces) <*> year

