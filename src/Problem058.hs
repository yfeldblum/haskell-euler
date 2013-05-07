--------------------------------------------------------------------------------
--  Problem 58
--------------------------------------------------------------------------------
--  What is the side length of the square spiral for which the ratio of primes
--  along both diagonals to numbers along both diagonals first falls below 10%?
--------------------------------------------------------------------------------
--  26241
--------------------------------------------------------------------------------

module Problem058 where

import Data.Ratio

import Base ( (|>) )
import Juke ( isPrime, primes'' )

solutionFrom [] = solution

solution = 2 * s + 1
	where s = fst $ head $ filter (\ (s, c) -> c < 0.1) $ groupedAccumFractionPrimes

steps = [2,4..]
repeatEach c [] = []
repeatEach c (x:xs) = replicate c x ++ repeatEach c xs

diags = tail $ scanl (+) 1 $ repeatEach 4 $ steps
groupedDiags = zip [1..] $ inGroupsOf 4 $ diags

inGroupsOf c xs = a : inGroupsOf c b where
	(a, b) = splitAt c xs

groupedCountPrimes = map countPrimes groupedDiags where
	countPrimes (i, ns) = (i, length $ filter (isPrime primes'') $ ns)

groupedAccumCountPrimes = go 0 groupedCountPrimes where
	go n ((s,c):gs) = (s, n + c) : go (n + c) gs

groupedAccumFractionPrimes = map (\ (s, c) -> (s, fromIntegral c / fromIntegral (s * 4 + 1))) groupedAccumCountPrimes

