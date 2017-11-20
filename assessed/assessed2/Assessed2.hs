{-# LANGUAGE Safe #-}

module Assessed2
    (isInTree, tlookup, rtlookup, insertion)
where

import safe Lib

-- Q1
isInTree :: Ord a => a -> Tree a -> Bool
isInTree _ Empty = False
isInTree x (Branch [] t') = isInTree x t'
isInTree x (Branch (t:ts) t') = if x == snd t then True else if x > snd t then isInTree x (Branch ts t') else isInTree x (fst t)

-- Q2
tlookup :: Ord k => k -> Tree (Record k v) -> Maybe v
tlookup x Empty = Nothing
tlookup x (Branch [] t') = tlookup x t'
tlookup x (Branch ((t,(Pair k v)):ts) t') = if x == k then Just v else if x > k then tlookup x (Branch ts t') else tlookup x t

-- Q3
rtlookup :: Ord k => (k,k) -> Tree (Record k v) -> [v]
rtlookup x Empty = []
rtlookup (l, m) (Branch ((t,(Pair k v)):ts) t') = if l <= k && k <= m
                                                  then rtlookup (l, m) t
                                                  else rtlookup (l,m) (Branch ts t')

-- Q4
insertion :: Ord a => (a,a) -> Tree a -> Tree a
insertion = undefined