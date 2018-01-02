module ExpressionParser (parseExpr, module Expr, module Parser, module UtilsParsers) where

import Expr
import Parser
import UtilsParsers

-- | Parses an expression
parseExpr :: Parser (Expr Name)
parseExpr = parseLocalDef
            <|> parseRecursiveDef
            <|> parseCase
            <|> parseLambda
            <|> parseAExpr


-- | Parses a local definition
parseLocalDef :: Parser (Expr Name)
parseLocalDef = do symbol "let"
                   defs <- parseMultipleDefs
                   symbol "in"
                   expr <- parseExpr
                   return (ELet NonRecursive defs expr)

-- | Parses a recursive definition
parseRecursiveDef :: Parser (Expr Name)
parseRecursiveDef = do symbol "letrec"
                       defs <- parseMultipleDefs
                       symbol "in"
                       expr <- parseExpr
                       return (ELet Recursive defs expr)

-- | Parses multiple definitions
parseMultipleDefs :: Parser ([Def Name])
parseMultipleDefs = do d <- parseDef
                       df <- many (do symbol ";"
                                      parseDef)
                       return (d:df)

-- | Parses a definition
parseDef :: Parser (Def Name)
parseDef = do i <- identifier   -- Get the identifier
              symbol "="        -- Read the "="
              v <- parseExpr    -- Get the value
              return (i, v)     -- Return (identifier, value)

-- | Parses a case expression
parseCase :: Parser (Expr Name)
parseCase = do symbol "case"
               exp <- parseExpr
               symbol "of"
               alts <- parseAlts
               return (ECase exp alts)

-- | Parsers one or more alternatives
parseAlts :: Parser ([Alter Name])
parseAlts = do a <- parseAlt
               ass <- many (do parseAlt)
               return (a:ass)


-- | Parses a single alternative
parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              index <- natural
              symbol ">"
              vars <- many identifier
              symbol "->"
              exp <- parseExpr
              return (index, vars, exp)

-- | Parses a lambda expression
parseLambda :: Parser (Expr Name)
parseLambda = do symbol "\005C"
                 vars <- some identifier
                 symbol "\002E"
                 exp <- parseExpr
                 return (ELam vars exp)

-- | Parses an atomic expression
parseAExpr :: Parser (Expr Name)
parseAExpr = parseVar
             <|> parseNum
             <|> parseConst
             <|> parseParExpr

-- | Parsers a variable
parseVar :: Parser (Expr Name)
parseVar = do i <- identifier
              return (EVar i)


-- | Parsers a number
parseNum :: Parser (Expr Name)
parseNum = do n <- integer
              return (ENum n)

-- | Parsers a constructor
parseConst :: Parser (Expr Name)
parseConst = do symbol "Pack"
                symbol "{"
                tag <- natural
                symbol ","
                arity <- natural
                symbol "}"
                return (EConstr tag arity)


-- | Parses an parenthesized expression
parseParExpr :: Parser (Expr Name)
parseParExpr = do symbol "("
                  e <- parseExpr
                  symbol ")"
                  return e
