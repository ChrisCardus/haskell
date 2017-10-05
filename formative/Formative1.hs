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


fromJust :: Maybe Char -> String
fromJust (Just a) = [a]
fromJust Nothing = ""


findLetter :: [MorseUnit] -> [MorseUnit]
findLetter xs | xs == [] = []
              | take 4 xs == [Beep, Beep, Beep, Silence] = dah ++ findLetter (drop 4 xs)
              | take 2 xs == [Beep, Silence] = dit ++ findLetter (drop 2 xs)
              | xs /= [] = []

findGap :: [MorseUnit] -> [MorseUnit]
findGap xs | take 4 xs == [Silence, Silence, Silence, Silence] = mediumGap
          | take 2 xs == [Silence, Silence] = shortGap
          | xs /= [] || xs == [] = []


encode :: String -> [MorseUnit]
encode [] = []
encode (x:xs) = if x /= ' ' then codeSymbol (toUpper x) ++ shortGap ++ encode (xs) else mediumGap ++ encode (xs)


decode :: [MorseUnit] -> String
decode [] = ""
decode xs | lookup (findLetter xs) morseTable /= Nothing = fromJust (lookup (findLetter xs) morseTable) ++ decode (drop (length (findLetter xs)) xs)
          | findGap xs == mediumGap = " " ++ decode (drop (length mediumGap) xs)
          | findGap xs == shortGap = "" ++ decode (drop (length shortGap) xs)
          | xs /= [] = ""


toTree :: MorseTable -> MorseTree
toTree = undefined


toTable :: MorseTree -> MorseTable
toTable = undefined