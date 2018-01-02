module ReadFile where

import System.IO

readF :: IO String
readF = do inh <- openFile "input.txt" ReadMode
           prog <- readloop inh
           hClose inh
           return prog

readloop inh = do ineof <- hIsEOF inh
                  if ineof
                     then return []
                  else do
                     x <- hGetLine inh
                     xs <- readloop inh
                     return (x++xs)

