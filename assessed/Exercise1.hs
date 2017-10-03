fsthalf :: [a] -> [a]
fsthalf xs = take (length xs `div` 2) xs

sndhalf :: [a] -> [a]
sndhalf xs = drop (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

{- My original implementation (which was bubble sort...)
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (x:xs) = merge [x] (msort xs)-}

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fsthalf xs)) (msort (sndhalf xs))

{- My horribly inefficient original implementation
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted xs = if xs == msort xs
              then True
              else False-}

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs [] = False
isPermutation [] ys = False
isPermutation xs ys = msort(xs) == msort(ys)