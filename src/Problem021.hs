--------------------------------------------------------------------------------
--  Problem 21
--------------------------------------------------------------------------------
--  Let d(n) be defined as the sum of the proper divisors of n. Let (a, b) be an
--  amicable pair if d(a) = b and d(b) = a but a /= b, and let a, b be amicable
--  numbers. Evaluate the sum of all the amicable numbers under 10000.
--------------------------------------------------------------------------------
--  31626
--------------------------------------------------------------------------------

module Problem021 where

import Juke (factors')

solutionFrom [] = solutionFrom ["10000"]
solutionFrom [maxnS] = return $ show $ solutionGen (read maxnS)

solutionGen maxn = sum $ filter amicable $ [1 .. maxn - 1]

amicable n = dn /= n && ddn == n where
	dn = d n
	ddn = d dn

d n = (sum $ factors' $ n) - n
