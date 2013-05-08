--------------------------------------------------------------------------------
--  Problem 9
--------------------------------------------------------------------------------
--  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
--  Find the product abc.
--------------------------------------------------------------------------------
--  31875000
--------------------------------------------------------------------------------

module Problem009 where

solutionFrom [] = return $ show $ solution

solution = head products where
	products = [ a * b * c | (a, b, c) <- tuples ]
	tuples = [ (a, b, c) | a <- nums, b <- nums, let c = tsum - (a + b), c >= 0, a^2 + b^2 == c^2 ]
	(tsum, nums) = (1000, [1 .. tsum])
