--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Higher-order functions                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE MultiWayIf #-}

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( elem, maximum, intersperse, transpose
                      , subsequences, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem, id )

import Data.List (delete)

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

-- a -> (b -> c). Yes, this returns a function
-- (a -> b) -> Int. No.
-- (Int, Bool) -> Char. No.
-- a -> a. `id id`

-- not
-- null
-- filter 
-- (.)

elem :: Eq a => a -> [a] -> Bool
-- elem x ys = (not . null . filter (==x)) ys
elem x = not . null . filter (==x)

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y r -> x==y || r) False

maximum :: Ord a => [a] -> a
maximum = foldr1 max

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs

flip :: (a -> b -> c) -> b -> a -> c
flip f y x = f x y

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) 
    | p x = x : takeWhile p xs
    | otherwise = []

-- takeWhile _ [] = []
-- takeWhile p (x:xs) = 
--     if p x 
--     then x : takeWhile p xs 
--     else []

-- Requires MultiWayIf:
-- takeWhile _ [] = []
-- takeWhile p (x:xs) = 
--     if | p x       -> x : takeWhile p xs 
--        | otherwise -> []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zip :: [a] -> [b] -> [(a,b)]
zip = zipWith (\x y -> (x,y))
-- zip = zipWith (,)

--    groupBy (==) [1,2,2,3,4,4,1] 
-- => [[1], [2,2], [3], [4,4], [1]]
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy r (x:xs) = (x : ys) : groupBy r zs
    where (ys, zs) = span (r x) xs

-- span p xs = (takeWhile p xs, dropWhile p xs)

-- permutations "abc"
-- => ["abc", "acb", "bac", "bca", "cab", "cba"]
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = 
    [x:ys | x <- xs
          , ys <- permutations (delete x xs)
          ]

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = go xs []
    where go :: [a] -> [a] -> [[a]]
          go []     _  = []
          go (y:ys) zs = map (y:) (permutations' (zs++ys)) 
                      ++ go ys (y:zs)

--------------------------------------------------------------------------------
