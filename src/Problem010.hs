--------------------------------------------------------------------------------
--  Problem 10
--------------------------------------------------------------------------------
--  Calculate the sum of all the primes below two million.
--------------------------------------------------------------------------------
--  142913828922
--------------------------------------------------------------------------------

module Problem010 where

import Juke (primes'')

solutionFrom [] = solutionFrom ["2000000"]
solutionFrom [maxnS] = return $ show $ solutionGen (read maxnS)

solutionGen maxn = sum $ takeWhile (< maxn) $ primes''
