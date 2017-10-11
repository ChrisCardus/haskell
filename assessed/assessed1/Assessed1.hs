{-# LANGUAGE Safe #-}

module Assessed1
    (expr2comb,comb2expr,eval,evalVar,evalVar',showExpr)
where

import safe Lib 

-- Q1
expr2comb :: BoolExpr -> BoolComb Bool
expr2comb = undefined

comb2expr :: (BoolComb Bool) -> BoolExpr
comb2expr = undefined

-- Q2
eval :: BoolComb Bool -> Bool
eval = undefined
-- Q3
evalVar :: BoolComb (Either String Bool) -> [(String , Bool)] -> Maybe Bool
evalVar = undefined

evalVar' :: BoolComb (Either String Bool) -> (String -> Maybe Bool) -> Maybe Bool
evalVar' = undefined

-- Q4
showExpr :: BoolComb Char -> String
showExpr = undefined 