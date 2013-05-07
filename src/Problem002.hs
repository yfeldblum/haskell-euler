--------------------------------------------------------------------------------
--  Problem 2
--------------------------------------------------------------------------------
--  Find the sum of all the even-valued terms in the Fibonacci sequence which do
--  not exceed four million.
--------------------------------------------------------------------------------
--  4613732
--------------------------------------------------------------------------------

module Problem002 where

import Juke (fibonaccis)

solutionFrom [] = solutionFrom ["4000000"]
solutionFrom [maxnS] = solutionGen (read maxnS)

solutionGen maxn = sum $ filter even $ takeWhile (< maxn) $ fibonaccis
