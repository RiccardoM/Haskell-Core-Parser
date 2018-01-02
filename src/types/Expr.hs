module Expr where

type Name = String

data Expr a
    =   EVar Name               -- Variables
      | ENum Int                -- Numbers
      | EConstr Int Int         -- Constructor (Tag, Arity)
      | EAp (Expr a) (Expr a)   -- Applications
      | ELet                    -- Let (rec) expressions
            IsRec               --      boolean with True = recursive
            [Def a]             --      Definitions
            (Expr a)            --      Body of let (rec)
      | ECase                   -- Case expressions
            (Expr a)            --      Expression to scrutinise
            [Alter a]           --      Alternatives
      | ELam [a] (Expr a)       -- Lambda abstractions

    deriving Show


type Def a = (a, Expr a)            -- For let (rec)
type Alter a = (Int, [a], Expr a)   -- For case

data IsRec = NonRecursive | Recursive
    deriving Show

