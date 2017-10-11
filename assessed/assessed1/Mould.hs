{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP  #-}
{-

This module will import your solutions, check that they have the right type,
and re-export your solutions.

-}

module Mould
    (expr2comb,comb2expr,eval,evalVar,evalVar',showExpr)
where

import safe Lib

import safe qualified Assessed1 as StudentSolution

-- Q1
expr2comb :: BoolExpr -> BoolComb Bool
expr2comb = StudentSolution.expr2comb

comb2expr :: (BoolComb Bool) -> BoolExpr
comb2expr = StudentSolution.comb2expr

-- Q2
eval :: BoolComb Bool -> Bool
eval = StudentSolution.eval

-- Q3
evalVar :: BoolComb (Either String Bool) -> [(String , Bool)] -> Maybe Bool
evalVar = StudentSolution.evalVar

evalVar' :: BoolComb (Either String Bool) -> (String -> Maybe Bool) -> Maybe Bool
evalVar' = StudentSolution.evalVar'

-- Q4
showExpr :: BoolComb Char -> String
showExpr = StudentSolution.showExpr
