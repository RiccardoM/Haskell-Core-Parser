module ScDefn(module ScDefn, module Expr) where

import Expr

type ScDefn a = (Name, [a], Expr a)  -- Super combinator definition
type CoreScDefn = ScDefn Name

