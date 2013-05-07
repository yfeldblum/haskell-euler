--------------------------------------------------------------------------------
--  Problem 12
--------------------------------------------------------------------------------
--  What is the value of the first triangle number to have over five hundred
--  divisors?
--------------------------------------------------------------------------------
--  76576500
--------------------------------------------------------------------------------

module Problem012 where

import Juke (multiFactors)

solutionFrom [] = solutionFrom ["500"]
solutionFrom [maxnS] = solutionGen (read maxnS)

solutionGen maxn = fst $ head $ filter tflValid $ map tfactors $ tnumbers where
	tflValid = \ (n, cnt) -> cnt > maxn
	tfactors = \ n -> (n, product $ map (+ 1) $ map snd $ multiFactors n)
	tnumbers = scanl1 (+) [0..]
