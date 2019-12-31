--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Higher-order functions                                                --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( elem, maximum, intersperse, transpose
                      , subsequences, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem )

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

elem :: Eq a => a -> [a] -> Bool
elem = undefined

maximum :: Ord a => [a] -> a
maximum = undefined

any :: (a -> Bool) -> [a] -> Bool
any = undefined

all :: (a -> Bool) -> [a] -> Bool
all = undefined

flip :: (a -> b -> c) -> b -> a -> c
flip = undefined

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy = undefined

permutations :: Eq a => [a] -> [[a]]
permutations = undefined

--------------------------------------------------------------------------------
