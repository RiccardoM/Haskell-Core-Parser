module UtilsParsers(module UtilsParsers, module Parser, module Data.Char) where

import Data.Char
import Parser

--------------------------
--- DERIVED PRIMITIVES ---
--------------------------

-- | Parses the first character of the input, taking the remaining chars into a list
item :: Parser Char
item = P (\inp -> case inp of
                    [] ->     []
                    (x:xs) -> [(x, xs)])

-- | Parses the first character if it satisfies the conditions, and returns empty otherwise
sat :: (Char -> Bool) -> Parser Char
sat predicate = do x <- item
                   if predicate x then return x else empty

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


-- | Parses an identifiers (variable names)
ident :: Parser String
ident = do v <- lower
           vs <- many alphanum
           return (v:vs)

-- | Parses a natural number
nat :: Parser Int
nat = do ns <- some digit
         return (read ns) -- Converts a string to an integer

-- | Parsers zero or more spaces
space :: Parser ()
space = do many (sat isSpace) -- Takes all the spaces
           return ()          -- Returns the remaining string

-- | Parses an integer number
int :: Parser Int
int = do char '-'    -- If it reads the -
         n <- nat        -- Keep reading the number
         return (-n)     -- Return the negative number
      <|> nat -- Otherwise return the simple number


-----------------------------------------------------------------------------------------------------------------------

-----------------------
--- HANDLING SPACES ---
-----------------------


-- | Define a function that ignores spaces between and after applying a parser
token :: Parser a -> Parser a
token parser = do space
                  value <- parser
                  space
                  return value

-- Define parsers that ignore spaces around identifiers, natural numbers, integers and special symbols

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)


-- | Parses a string that represents a list of natural numbers written inside two square brackets and separated by a
-- | comma
nats :: Parser [Int]
nats = do symbol "["                    -- Read the [ parenthesis
          n <- natural                  -- Read the first natural value
          ns <- many (do symbol ","      -- Read multiple comma - value pairs and get the natural value
                         natural)
          symbol "]"                    -- Read the ] parenthesis
          return (n:ns)                 -- Return the list of read values

