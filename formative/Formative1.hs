-- This header must be kept exactly as it is for your submission to be valid:

{-# LANGUAGE Safe #-} 


module Formative1
    (encode, decode, toTree, toTable)
where

import safe Lib

-- End of header.

isLower :: Char -> Bool
isLower c = elem c ['a'..'z']


toUpper :: Char -> Char
toUpper c = if isLower c then toEnum (fromEnum c - 32) else c


{-isDit :: [MorseUnit] -> Bool
isDit [] = False
isDit (x:y:xs) = if x == Beep && y == Silence then True else False-}


encode :: String -> [MorseUnit]
encode [] = []
encode (x:xs) = if x /= ' ' then codeSymbol (toUpper x) ++ shortGap ++ encode (xs) else mediumGap ++ encode (xs)


decode :: [MorseUnit] -> String
decode [] = []
decode (x:xs) = 


toTree :: MorseTable -> MorseTree
toTree = undefined


toTable :: MorseTree -> MorseTable
toTable = undefined