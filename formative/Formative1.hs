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


encode :: String -> [MorseUnit]
encode [] = []
encode (x:xs) = if x /= ' ' then codeSymbol (toUpper x) ++ shortGap ++ encode (xs) else mediumGap ++ encode (xs)


fromJust :: Maybe Char -> String
fromJust (Just a) = [a]
fromJust Nothing = ""

{-fromJust' :: Maybe Char -> Char
fromJust' (Just a) = a
fromJust' Nothing = ' '-}


findLetter :: [MorseUnit] -> [MorseUnit]
findLetter xs | xs == [] = []
              | take 4 xs == [Beep, Beep, Beep, Silence] = dah ++ findLetter (drop 4 xs)
              | take 2 xs == [Beep, Silence] = dit ++ findLetter (drop 2 xs)
              | xs /= [] = []


findGap :: [MorseUnit] -> [MorseUnit]
findGap xs | take 4 xs == [Silence, Silence, Silence, Silence] = mediumGap
           | take 2 xs == [Silence, Silence] = shortGap
           | xs /= [] || xs == [] = []


decode :: [MorseUnit] -> String
decode [] = ""
decode xs | lookup (findLetter xs) morseTable /= Nothing = fromJust (lookup (findLetter xs) morseTable) ++ decode (drop (length (findLetter xs)) xs)
          | findGap xs == mediumGap = " " ++ decode (drop (length mediumGap) xs)
          | findGap xs == shortGap = "" ++ decode (drop (length shortGap) xs)
          | xs /= [] = ""


data Direction = L | R deriving (Show, Eq, Ord)
type Address   = [Direction]


address :: [MorseUnit] -> Address
address xs | xs == [] = []
           | take 4 xs == [Beep, Beep, Beep, Silence] = [R] ++ address (drop 4 xs)
           | take 2 xs == [Beep, Silence] = [L] ++ address (drop 2 xs)
           | otherwise = []


addressList :: MorseTable -> [(Address, Char)]
addressList [] = []
addressList (x:xs) = ((address (fst x)), (snd x)) : addressList xs


{-sortAddressList :: [Address] -> [Address]
sortAddressList [] = []
sortAddressList (x:xs) = sortAddressList smaller ++ [x] ++ sortAddressList larger
                         where
                            smaller = [a | a <- xs, a <= x]
                            larger = [b | b <- xs, b > x]-}


--Cycle through each location in the tree breadth first and check if the address of said location exists within addressList. If it does, Branch1, if it doesn't, Branch0. End when the address list is empty.
toTree :: MorseTable -> MorseTree
toTree xs = toTree' xs [] (addressList morseTable)


removeItem :: Address -> [(Address, Char)] -> [(Address, Char)]
removeItem _ [] = []
removeItem x (y:ys) | x == (fst y)    = ys
                    | otherwise = y : removeItem x ys


findChar :: Address -> [(Address, Char)] -> Char
findChar _ [] = ' '
findChar x (y:ys) | x == (fst y) = (snd y)
                  | otherwise = findChar x ys


toTree' :: MorseTable -> Address -> [(Address, Char)] -> MorseTree
toTree' [] [] _ = Branch0 Nil Nil
toTree' xs [] zs = Branch0 (toTree' xs [L] zs) (toTree' xs [R] zs)
toTree' xs ys zs = if elem ys (map fst zs) && (elem (ys ++ [L]) (map fst zs) || elem (ys ++ [R]) (map fst zs))
                   then Branch1 (findChar ys zs) (if elem (ys ++ [L]) (map fst zs) || elem (ys ++ [L,L]) (map fst zs) || elem (ys ++ [L,R]) (map fst zs) then toTree' xs (ys ++ [L]) (removeItem ys zs) else Nil) (if elem (ys ++ [R]) (map fst zs) || elem (ys ++ [R,L]) (map fst zs) || elem (ys ++ [R,R]) (map fst zs) then toTree' xs (ys ++ [R]) (removeItem ys zs) else Nil)
                   else if elem ys (map fst zs) && not (elem (ys ++ [L]) (map fst zs)) && not (elem (ys ++ [R]) (map fst zs)) && not (elem (ys ++ [L,R]) (map fst zs)) && not (elem (ys ++ [R,R]) (map fst zs)) && not (elem (ys ++ [L,L]) (map fst zs)) && not (elem (ys ++ [R,L]) (map fst zs))
                   then Leaf (findChar ys zs)
                   else Branch0 (if elem (ys ++ [L]) (map fst zs) || elem (ys ++ [L,L]) (map fst zs) || elem (ys ++ [L,R]) (map fst zs) then toTree' xs (ys ++ [L]) zs else Nil) (if elem (ys ++ [R]) (map fst zs) || elem (ys ++ [R,L]) (map fst zs) || elem (ys ++ [R,R]) (map fst zs) then toTree' xs (ys ++ [R]) zs else Nil)


toTable :: MorseTree -> MorseTable
toTable = undefined