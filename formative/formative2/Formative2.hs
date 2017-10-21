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
height = undefined


p0         :: Ord a => Tree a -> Bool
p1, p2, p3 ::          Tree a -> Bool

p0 = undefined
p1 = undefined
p2 = undefined
p3 = undefined


smins :: Ord a => a -> Tree a -> Tree a
smins = undefined


delete :: Ord a => a -> Tree a -> Tree a
delete = undefined
