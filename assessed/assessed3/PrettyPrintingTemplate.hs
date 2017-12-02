module PrettyPrinting where


import Unsolved
import AbstractSyntax

spaces n = take n (repeat ' ')

ppExpr :: Expr -> String
ppExpr = question "pretty print expressions"

ppProgram :: Program -> (Int -> String)
ppProgram = question "pretty print programs"


