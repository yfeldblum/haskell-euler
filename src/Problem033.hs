--------------------------------------------------------------------------------
--  Problem 33
--------------------------------------------------------------------------------
--  A "curious fraction" is a fraction such as 49/98 where the correct answer
--  may be found using the incorrect method of choosing a digit appearing in
--  both the numerator and the denominator and deleting it from both. There are
--  exactly four non-trivial "curious fractions" between 0 and 1 in value having
--  two-digit numerators and denominators. If the product of these four frac-
--  tions is given in lowest common terms, find the value of the denominator. 
--------------------------------------------------------------------------------
--  100
--------------------------------------------------------------------------------

module Problem033 where

import Data.List (delete)

import Juke (ltoi, itol)

solutionFrom [] = solutionFrom ["10"]
solutionFrom [baseS] = solutionGen (read baseS)

solutionGen base = snd $ reduce $ foldl mult (1, 1) $ fractions base

mult (a, b) (a', b') = (a * a', b * b')

reduce (a, b) = (a `quot` c, b `quot` c) where
	c = gcd a b

fractions base =
	[ (a, b)
	| a <- [base .. base ^ 2 - 2]
	, b <- [a + 1 .. base ^ 2 - 1]
	, a `rem` base /= 0 || b `rem` base /= 0
	, isCurious base a b
	]

isCurious base a b = or $
	[ a * cancel base i b == b * cancel base i a
	| i <- [1 .. base - 1]
	, i `elem` itol base a
	, i `elem` itol base b
	] where
	cancel base k n = ltoi base $ delete k $ itol base $ n
