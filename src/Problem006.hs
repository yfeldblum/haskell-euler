--------------------------------------------------------------------------------
--  Problem 6
--------------------------------------------------------------------------------
--  Find the difference between the sum of the squares of the first one hundred
--  natural numbers and the square of the sum.
--------------------------------------------------------------------------------
--  25164150
--------------------------------------------------------------------------------

module Problem006 where

solutionFrom [] = solutionFrom ["100"]
solutionFrom [maxnS] = return $ show $ solutionGen (read maxnS)

solutionGen maxn = abs $ (sum $ map square $ nums) - (square $ sum $ nums) where
	nums = [1 .. maxn]
	square = (^ 2)
