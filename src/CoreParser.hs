module CoreParser(module CoreParser, module Program, module ExpressionParser) where

import Program
import ExpressionParser

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do symbol ";"
                  ps <- parseProg
                  return (p:ps)
                <|> return [p]

parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- identifier           -- Get the name of the variable
                pf <- many identifier     -- Get all the variables used
                symbol "="                 -- Read the "=" symbol
                body <- parseExpr          -- Parse the expressions that must be assigned
                return (v, pf, body)
