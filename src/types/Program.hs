module Program(module Program, module ScDefn) where

import ScDefn

type Program a = [ScDefn a]
type CoreProgram = Program Name

