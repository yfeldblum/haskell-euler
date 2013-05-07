--------------------------------------------------------------------------------
--  Problem 14
--------------------------------------------------------------------------------
--  The following iterative sequence is defined for the set of positive
--  integers:
--    n -> n / 2 (n is even)
--    n -> 3n + 1 (n is odd)
--  Which starting number under one million produces the longest chain ending at
--  one?
--------------------------------------------------------------------------------
--  837799
--------------------------------------------------------------------------------

module Problem014 where

import Data.List (foldl1')

solutionFrom [] = solutionFrom ["1000000"]
solutionFrom [maxnS] = solutionGen (read maxnS)

{-
solutionGen maxn = fst $ maximumx pred $ map pair $ [1 .. maxn - 1] where
	pair n = (n, g n)
	pred p q = seq p' $ seq q' $ p' < q' where
		p' = snd p
		q' = snd q
-}
solutionGen maxn = fst $ foldl1' max $ map pair $ [1 .. maxn - 1] where
	pair n = (n, g n)
	max p q = if snd p < snd q then q else p

f n | n `rem` 2 == 0 = n `quot` 2
f n | otherwise      = 3 * n + 1

--  g n: The length of the chain generated by f n.
g n = go 1 n where
	go k 1 = k
	go k n = go (k + 1) (f n)