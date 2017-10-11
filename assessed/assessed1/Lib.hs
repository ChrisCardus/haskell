-- This file is automatically generated by mdtohs.hs,
-- and hence should not be changed manually.

-- The following code will appear in Lib.hs, which is
-- generated from this README.md file.
--

-- This file Lib.hs is imported in Formative1.hs .

{-# LANGUAGE Safe #-}

module Lib where


data BoolComb t = And (BoolComb t) (BoolComb t) |
                Or (BoolComb t) (BoolComb t) |
                Not (BoolComb t) |
                Atom t
                deriving (Show, Eq)


data BoolExpr = BAnd BoolExpr BoolExpr |
                BOr BoolExpr BoolExpr  |
                BNot BoolExpr |
                BTrue |
                BFalse
                deriving (Show, Eq)


tt :: BoolComb Bool
ff :: BoolComb Bool
tt = Atom True
ff = Atom False


andS :: String
andS = " && "

orS :: String
orS = " || "

notS :: String
notS = "¬ "

