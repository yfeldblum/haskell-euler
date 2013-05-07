--------------------------------------------------------------------------------
--  Problem 53
--------------------------------------------------------------------------------
--  How many, not necessarily distinct, values of n-choose-r, for 1 <= n <= 100,
--  are greater than 1,000,000?
--------------------------------------------------------------------------------
--  4075
--------------------------------------------------------------------------------

module Problem053 where

import Juke ( partitions )

solutionFrom [] = solutionFrom ["100", "1000000"]
solutionFrom [ncapS, valminS] = solutionGen (read ncapS) (read valminS)

solutionGen ncap valmin = fromIntegral $ length values
	where
	values =
		[ p
		| n <- [1..ncap]
		, r <- [0..n]
		, let p = partitions [r, n-r]
		, p > valmin
		]
