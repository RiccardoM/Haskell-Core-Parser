module Main where

import ReadFile
import CoreParser


main :: IO (Program Name)
main = do inp <- readF
          return (comp (parse parseProg inp)) -- Call to parseProg


comp :: [(Program Name, Name)] -> Program Name
comp [] = error "Nothing to parse"
comp [(e, [])] = e
comp [(_, a)] = error ("Doesn't use all input. Remainings: " ++ a)
