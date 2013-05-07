--------------------------------------------------------------------------------
--  Problem 71
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  428570
--------------------------------------------------------------------------------

module Problem071 where

import Data.Ratio

import Base ( (|>), on )

solutionFrom [] = solution

solution = solutionGen (3%7) 1000000

solutionGen frac maxd = numerator $ maximum $ go (0,1)
	where
	go (n, d) | d > maxd = []
	go (n, d) | otherwise = n' % d' : go (n', d')
		where
		d' = d + 1
		n' = if (n + 1) % d' < frac then n + 1 else n
