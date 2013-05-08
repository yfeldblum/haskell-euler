--------------------------------------------------------------------------------
--  Problem 26
--------------------------------------------------------------------------------
--  Find the value of d < 1000 for which 1 / d contains the longest recurring
--  cycle in its decimal fraction part.
--------------------------------------------------------------------------------
--  983
--------------------------------------------------------------------------------

module Problem026 where

import Base
import Juke (fractionExpansion, primes)

solutionFrom [] = solutionFrom ["10", "1000"]
solutionFrom [baseS, maxnS] = return $ show $ solutionGen' (read baseS :: Integer) (read maxnS)

solutionGen base maxn = fst $ maximumFrom pred $ divns where
	divns = [ (i, fractionExpansion base 1 i) | i <- takeWhile (< maxn) primes ]
	pred n n' = length (extract n) < length (extract n') where
		extract (_, (_, _, r)) = r

solutionGen' base maxn = fst $ maximumFrom pred $ periods where
	periods = [ (n, period base n) | n <- nums ]
	nums = takeWhile (< maxn) $ [ n | n <- [3, 5 ..], n `rem` 5 /= 0 ]
	pred n n' = snd n < snd n'

period base n = head $ [ p | p <- [1 ..], (10 ^ p - 1) `rem` n == 0 ]

maximumFrom pred = foldl1 (maxFrom pred)

maxFrom pred a b = if pred a b then b else a
