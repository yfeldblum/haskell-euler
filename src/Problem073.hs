--------------------------------------------------------------------------------
--  Problem 73
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  7295372
--------------------------------------------------------------------------------

module Problem073 where

import Data.Ratio

import Base ( (|>), on )

solutionFrom [] = solutionFrom ["12000"]
solutionFrom [maxbS] = solutionGen (read maxbS)

solutionGen maxb =
	[1..maxb]
	|>	map (numFractions (1%3) (1%2))
	|>	sum
	|>	fromIntegral

numFractions p q d = length [ n | n <- [minn d .. maxn d], n % d > p, n % d < q, gcd n d == 1 ] where
	minn d = (numerator p * d + 1) `quot` denominator p
	maxn d = (numerator q * d - 1) `quot` denominator q
