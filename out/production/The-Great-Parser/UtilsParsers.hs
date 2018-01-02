module UtilsParsers where

import Data.Char
import Parser


-- | Parses the first character of the input, taking the remaining chars into a list
item :: Parser Char
item = P (\inp -> case inp of
                    [] ->     []
                    (x:xs) -> [(x, xs)])

-- | Parses the first character if it satisfies the conditions, and returns empty otherwise
sat :: (Char -> Bool) -> Parser Char
sat predicate = do x <- item
                   if predicate x then return x else P(\inp -> []) -- This should be replaced with `empty`

-- |  Parses a single digit
digit :: Parser Char
digit = sat isDigit

-- | Parses a lowercase letter
lower :: Parser Char
lower = sat isLower

-- | Parses an uppercase letter
upper :: Parser Char
upper = sat isUpper

-- | Parses a letter
letter :: Parser Char
letter = sat isAlpha

-- | Parses a letter or a digit
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- | Parses a specific character
char :: Char -> Parser Char
char x = sat (== x)

-- | Parses a string
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x       -- Get the first char
                   string xs     -- Recursive call
                   return (x:xs) -- Return the complete string

-- As Parser is an instance of Alternative, we get two more functions for free.
-- The first is `many p` which applies a parser `p` as many times (0+) as possible until it fails.
-- The second is `some p` which applies a parser `p` as many times (1+) as possible until it fails.


