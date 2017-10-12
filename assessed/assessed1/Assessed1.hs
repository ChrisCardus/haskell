{-# LANGUAGE Safe #-}

module Assessed1
    (expr2comb,comb2expr,eval,evalVar,evalVar',showExpr)
where

import safe Lib 

-- Q1
expr2comb :: BoolExpr -> BoolComb Bool
expr2comb be | be == BTrue = Atom True
             | be == BFalse = Atom False
expr2comb (BNot be) = Not (expr2comb be)
expr2comb (BOr b1 b2) = Or (expr2comb b1) (expr2comb b2)
expr2comb (BAnd b1 b2) = And (expr2comb b1) (expr2comb b2)

comb2expr :: (BoolComb Bool) -> BoolExpr
comb2expr bc | bc == Atom True = BTrue
             | bc == Atom False = BFalse
comb2expr (Not bc) = BNot (comb2expr bc)
comb2expr (Or b1 b2) = BOr (comb2expr b1) (comb2expr b2)
comb2expr (And b1 b2) = BAnd (comb2expr b1) (comb2expr b2)

-- Q2
eval :: BoolComb Bool -> Bool
eval bc | bc == Atom True = True
        | bc == Atom False = False
eval (Not bc) = if (eval bc) == False then True else False
eval (Or b1 b2) = if (eval b1) == True || (eval b2) == True then True else False
eval (And b1 b2) = if (eval b1) == True && (eval b2) == True then True else False

-- Q3
evalVar :: BoolComb (Either String Bool) -> [(String , Bool)] -> Maybe Bool
evalVar (Not bc) t = if (evalVar bc t) == Just False then Just True else Just False
evalVar (Or b1 b2) t = if (evalVar b1 t) == Just True || (evalVar b2 t) == Just True then Just True else Just False
evalVar (And b1 b2) t = if (evalVar b1 t) == Just True && (evalVar b2 t) == Just True then Just True else Just False
evalVar (Atom (Right e)) _ = Just e
evalVar (Atom (Left e)) t = if elem e (map fst t) then lookup e t else Nothing

evalVar' :: BoolComb (Either String Bool) -> (String -> Maybe Bool) -> Maybe Bool
evalVar' (Not bc) f = if (evalVar' bc f) == Just False then Just True else Just False
evalVar' (Or b1 b2) f = if (evalVar' b1 f) == Just True || (evalVar' b2 f) == Just True then Just True else Just False
evalVar' (And b1 b2) f = if (evalVar' b1 f) == Just True && (evalVar' b2 f) == Just True then Just True else Just False
evalVar' (Atom (Right e)) _ = Just e
evalVar' (Atom (Left e)) f = if f e == Just True then Just True else Just False

-- Q4
showExpr :: BoolComb Char -> String
showExpr (Not bc) = notS ++ (showExpr bc)
showExpr (Or b1 b2) = (showExpr b1) ++ orS ++ (showExpr b2)
showExpr (And b1 b2) = (showExpr b1) ++ andS ++ (showExpr b2)
showExpr (Atom c) = [c]