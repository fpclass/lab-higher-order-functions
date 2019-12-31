--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Higher-order functions                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.List (nub,subsequences,groupBy,sort)

import qualified Lab as L

--------------------------------------------------------------------------------

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- | The main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "elem" $
        prop "determines whether elements belong to a list" $
            \(xs :: [Int]) -> \x -> L.elem x xs == not (null (filter (==x) xs))
    describe "maximum" $
        prop "finds the maximum in non-empty lists" $
            forAll (listOf1 arbitrary) $ \(xs :: [Int]) ->
            L.maximum xs == last (sort xs)
    describe "any" $ do
        prop "determines whether at least one element satisfies the predicate" $
            \(xs :: [Int]) -> L.any odd xs == not (null (filter odd xs))
    describe "all" $ do
        prop "determines whether all elements satisfy the predicate" $
            \(xs :: [Int]) -> L.all even xs == and (map even xs)
    describe "takeWhile" $ do
        prop "all elements of the resulting list satisfy the predicate" $
            \(xs :: [Int]) ->
            all even (L.takeWhile even xs)
        prop "the resulting list is a subsequence of the argument" $
            \(xs :: [Int]) ->
            L.takeWhile odd xs `elem` subsequences xs
    describe "zipWith" $ do
        prop "resulting list contains as many elements as the shortest argument" $
            \(xs :: [Int]) (ys :: [Int]) ->
            length (L.zipWith (+) xs ys) == min (length xs) (length ys)
        prop "behaves like zip when the function creates pairs" $
            \(xs :: [Int]) (ys :: [Char]) ->
            zip xs ys == L.zipWith (\x y -> (x,y)) xs ys
    describe "groupBy" $ do
        prop "resulting sublists contain elements which satisfy the same predicate" $
            \(xs :: [Int]) -> and [all (==x) xs | (x:xs) <- L.groupBy (==) xs]
        prop "the sum of lengths of the sublists is the length of the input" $
            \(xs :: [Int]) -> length xs == sum (map length $ L.groupBy (==) xs)
    describe "permutations" $ modifyMaxSize (const 10) $ do
        prop "produces the right number of permutations" $ \(xs :: [Int]) ->
            length (L.permutations xs) == fac (length xs)
        modifyMaxSize (const 5) $ prop "produces unique permutations" $
            \(xs :: [Int]) -> length (L.permutations (nub xs)) ==
                              length (nub (L.permutations (nub xs)))
        prop "permutations are all the same length as the input" $
            \(xs :: [Int]) ->
                all ((==) (length xs) . length) (L.permutations xs)

--------------------------------------------------------------------------------
