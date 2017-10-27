{-# LANGUAGE Safe #-}

module Assessed2
    (isInTree, tlookup, rtlookup, insertion)
where

import safe Lib

-- Q1
isInTree :: Ord a => a -> Tree a -> Bool
isInTree = undefined

-- Q2
tlookup :: Ord k => k -> Tree (Record k v) -> Maybe v
tlookup = undefined

-- Q3
rtlookup :: Ord k => (k,k) -> Tree (Record k v) -> [v]
rtlookup = undefined

-- Q4
insertion :: Ord a => (a,a) -> Tree a -> Tree a
insertion = undefined