--------------------------------------------------------------------------------
--  Problem 32
--------------------------------------------------------------------------------
--  Find the sum of all products whose multiplicand, multiplier and product can
--  be written as a 1 through 9 pandigital equation.
--------------------------------------------------------------------------------
--  45228
--------------------------------------------------------------------------------

module Problem032 where

import Data.List (nub)

import Juke (ltoi, permutations)

solutionFrom [] = solutionFrom ["9"]
solutionFrom [kS] = solutionGen (read kS)

solutionGen k = fromIntegral $ sum $ nub $ concat $ map (g k) $ permutations $ [1 .. k]

g len list =
	[ c
	| i <- [1 .. len - 2]
	, j <- [1 .. len - i]
	, let (f, f') = splitAt i list
	, let (g, h) = splitAt j f'
	, let a = ltoi 10 f
	, let b = ltoi 10 g
	, let c = ltoi 10 h
	, a * b == c
	]
