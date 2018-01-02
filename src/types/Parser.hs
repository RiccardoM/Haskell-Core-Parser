module Parser (module Parser, module Control.Applicative) where

import Control.Applicative

-- | Create a new Parser type using newtype in order to allow it to be made into instances of classes
newtype Parser a = P(String -> [(a, String)])

-- | Apply a generic Parser to an input string
parse :: Parser a -> String -> [(a, String)]
parse (P parser) input = parser input

-- | Make Parser an instance of Functor
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser = P(\input -> case parse parser input of
                            []                   -> []
                            [(value, remaining)] -> [(f value, remaining)])

-- | Make Parser an instance of Applicative
instance Applicative Parser where
  -- | Transforms a value into a parser that always succeds with this value as its results,
  -- | without consuming any of the input string
  -- pure :: a -> Parser a
  pure value = P (\input -> [(value, input)])

  -- | Applies a parser that returns a function to a parser that returns an argument to give
  -- | a parser that returns the result of applying the function to the argument, and only
  -- | succeds if all the components succeed.
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P(\input -> case parse pg input of
                            []        -> []
                            [(g, out)] -> parse (fmap g px) out)


-- | Make Parser an instance of Monad
instance Monad Parser where
  -- | The parser p >>= f fails if the application of the parser p to the input string inp fails,
  -- | and otherwise applies the function f to the result value v to give another parser f v, which
  -- | is then applied to the output string out that was produced by the first parser to give the
  -- | final result
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P(\inp -> case parse p inp of
                          [] -> []
                          [(v, remaining)] -> parse (f v) remaining)


-- | Make Parser an instance of Alternative
instance Alternative Parser where
  -- | Empty will represent a parser that always fails regardless the input.
  -- | In our case this is the parser which returns an empty list
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- | The choice operator will call the second parser if the first has failed, or return the first
  -- | result if the first parser has succeded
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P(\inp -> case parse p inp of
                        []         -> parse q inp
                        [(v, out)] -> [(v, out)])

  -- As Parser is an instance of Alternative, we get two more functions for free.
  -- The first is `many p` which applies a parser `p` as many times (0+) as possible until it fails.
  -- The second is `some p` which applies a parser `p` as many times (1+) as possible until it fails.
