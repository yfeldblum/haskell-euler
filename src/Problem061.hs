--------------------------------------------------------------------------------
--  Problem 61
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  28684
--------------------------------------------------------------------------------

module Problem061 where

import Data.List ( delete )
import Data.Array ( array, (!) )

import Base ( (|>) )
import Juke ( itol, ltoi, permutations )
import Sort ( quicksort )

solutionFrom [] = return $ show $ solution

solution = sum $ head $ sets

formula k n = (n * (k * n + 2 - k)) `quot` 2
formulae = map formula [0..]
series = map (\ f -> map f [0..]) formulae

interestingSeries = map (takeWhile (< 10000)) $ map (dropWhile (< 1000)) $ take 6 $ drop 1 $ series
interestingMerged = quicksort $ concat $ interestingSeries
interestingPaired = map (listToPair . itol 100) $ interestingMerged
	where
	listToPair [a,b] = (a,b)
interestingIndexL = takeWhile ((< 100) . fst) $ map (\ (i, ks) -> (i, map snd ks)) $ risingIndex fst interestingPaired
interestingIndexA = array (0, 99) $ interestingIndexL

{-
linearFormula      n = (n * (0 * n + 2)) `quot` 2
triangularFormula  n = (n * (1 * n + 1)) `quot` 2
squareFormula      n = (n * (2 * n - 0)) `quot` 2
pentagonalFormula  n = (n * (3 * n - 1)) `quot` 2
hexagonalFormula   n = (n * (4 * n - 2)) `quot` 2
heptagonalFormula  n = (n * (5 * n - 3)) `quot` 2
octagonalFormula   n = (n * (6 * n - 4)) `quot` 2
-}

sets =
	[ nums
	| a <- [0..99]
	, b <- interestingIndexA ! a
	, c <- interestingIndexA ! b
	, d <- interestingIndexA ! c
	, e <- interestingIndexA ! d
	, f <- interestingIndexA ! e
	, a `elem` (interestingIndexA ! f)
	, let set = [a, b, c, d, e, f]
	, let pairs = zip set (tail $ cycle set)
	, let nums = map (ltoi 100) $ map (\ (a,b) -> [a,b]) $ pairs
	, allDifferent nums
	, isSpreadFrom nums interestingSeries
	]

allDifferent [] = True
allDifferent (x:xs) = if x `elem` xs then False else allDifferent xs

isSpreadFrom [] bs = True
isSpreadFrom (a:as) bs = not (null bs') && not (null subSpreadFroms)
	where
	bs' = filter (a `elem`) $ bs
	subSpreadFroms = filter (isSpreadFrom as) $ map (`delete` bs) $ bs'

risingIndex :: Integral a => (e -> a) -> [e] -> [(a, [e])]
risingIndex f els = g 0 els where
	g i els = (i, xs) : g (i + 1) ys where
		(xs, ys) = span (\ e -> f e == i) els
