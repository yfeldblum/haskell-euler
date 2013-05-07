--------------------------------------------------------------------------------
--  Problem 52
--------------------------------------------------------------------------------
--  Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x
--  contain the same digits as x.
--------------------------------------------------------------------------------
--  142857
--------------------------------------------------------------------------------

module Problem052 where

import Data.List ( sort )

import Juke ( itol )

solutionFrom [] = solutionFrom ["10", "6"]
solutionFrom [baseS, topS] = solutionGen (read baseS) (read topS)

solutionGen base top = head $ filter (multiplesHaveSameDigits base top) $ [1..]

multiplesHaveSameDigits base top n =
	topMultipleHasSameLength && allEqual (multipleLsSorted)
	where
	topMultipleHasSameLength = length (last multipleLs) == length (head multipleLs)
	multiples = map (n *) [1 .. top]
	multipleLs = map (itol base) multiples
	multipleLsSorted = map sort multipleLs
	allEqual (x:[]) = True
	allEqual (x:y:xs) = x == y && allEqual (y:xs)
