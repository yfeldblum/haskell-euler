--------------------------------------------------------------------------------
--  Problem 1
--------------------------------------------------------------------------------
--  Find the sum of all the multiples of 3 or 5 below 1000.
--------------------------------------------------------------------------------
--  233168
--------------------------------------------------------------------------------

module Problem001 where

import Juke(coprime)

solutionFrom [] = solutionFrom ["1000"]
solutionFrom (maxnS:[]) = solutionFrom [maxnS, "3", "5"]
solutionFrom (maxnS:numsS) = solutionGen (read maxnS) (map read numsS)

solutionGen maxn nums = sum $ filter isKept $ [0 .. maxn - 1] where
	isKept k = not $ all (coprime k) $ nums

solutionOpt maxn = f 3 + f 5 - f (3 * 5) where
	f k = sum $ [0, k .. maxn - 1]
