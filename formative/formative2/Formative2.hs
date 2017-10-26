{-

*** Important ***

*Copy* this file to a new file

       Formative1.hs

and develop your submission in the file Formative1.hs.

Write your answers for Formative1 in this file, as per the
instructions in the file README.md.

Solve a question by replacing "undefined" by your own code. If you are
not solving a question, keep it undefined. Make sure your code
compiles before submitting to Canvas.

Don't forget to run the presubmit before you submit.

-}
-- This header must be kept exactly as it is for your submission to be valid:

{-# LANGUAGE Safe #-} 

module Formative2
    (height, p0, p1, p2, p3, smins, delete)
where

import safe Lib
import safe K   (k)

-- End of header.



height :: Tree a -> Int
height Empty = -1
height (Branch (t:_) _) = 1 + height (fst t)

{-isSorted :: Ord a => Tree a -> a -> Bool
isSorted Empty _ = True
isSorted (Branch [] x) y = isSorted x y
isSorted (Branch (t:ts) x) y = if isSorted (fst t) (snd t) then isSorted (Branch ts x) y else False-}

p0         :: Ord a => Tree a -> Bool
p1, p2, p3 ::          Tree a -> Bool
p0 = undefined
p1 Empty = True
p1 (Branch ts _) = if (length ts + 1) >= 2 && (length ts + 1) <= (2 * k + 1) then True else False
p2 = undefined
p3 = undefined


smins :: Ord a => a -> Tree a -> Tree a
smins = undefined


delete :: Ord a => a -> Tree a -> Tree a
delete = undefined
