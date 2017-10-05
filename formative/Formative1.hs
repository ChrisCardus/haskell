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


getUnit :: [MorseUnit] -> [MorseUnit]
getUnit xs | xs == [] = []
           | xs == [Silence, Silence, Silence, Silence, _] = [mediumGap]
           | xs == [Silence, Silence, Silence, _] = [shortGap]
           | xs == [Beep, Beep, Beep, Silence, _] = [dah]
           | xs == [Beep, Silence, _] = [dit]

getLetter :: [MorseUnit]
getLetter xs | getUnit xs == [dit] = 

encode :: String -> [MorseUnit]
encode [] = []
encode (x:xs) = if x /= ' ' then codeSymbol (toUpper x) ++ shortGap ++ encode (xs) else mediumGap ++ encode (xs)


decode :: [MorseUnit] -> String
decode [] = ""
decode xs = if ditOrDah xs == dit then 


toTree :: MorseTable -> MorseTree
toTree = undefined


toTable :: MorseTree -> MorseTable
toTable = undefined