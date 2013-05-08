--------------------------------------------------------------------------------
--  Problem 23
--------------------------------------------------------------------------------
--  Let d(n) be the sum of the proper divisors of n. Then a perfect number is a
--  number n such that d(n) = n. A deficient number is a number n such that
--  d(n) < n while an abundant number is a number n such that d(n) > n. It can
--  be shown that all integers greater than 28123 can be written as the sum of
--  two abundant numbers. Find the sum of all the positive integers which cannot
--  be written as the sum of two abundant numbers.
--------------------------------------------------------------------------------
--  4179871
--------------------------------------------------------------------------------

module Problem023 where

import Data.List (group)

import Juke (factors')

import qualified Rising

solutionFrom [] = solutionFrom ["28123"]
solutionFrom [maxnS] = return $ show $ solutionGen (read maxnS)

d n = (sum $ factors' $ n) - n

deficient n = d n < n
abundant n = d n > n
perfect n = d n == n

abundants = filter abundant $ [0 ..]

e `elemS` [] = False
e `elemS` (c:cs) =
	case compare e c of
		LT -> False
		EQ -> True
		GT -> e `Rising.elem` cs

isBad n = null $ filter (`Rising.elem` abundants) $ map (n -) $ takeWhile (<= n `quot` 2) $ abundants where

solutionGen maxn = sum $ filter isBad $ [0 .. maxn]
