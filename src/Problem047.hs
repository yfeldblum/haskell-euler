--------------------------------------------------------------------------------
--  Problem 47
--------------------------------------------------------------------------------
--  Find the first four consecutive integers to have four distinct prime
--  factors. What is the first of these numbers?
--------------------------------------------------------------------------------
--  134043
--------------------------------------------------------------------------------

module Problem047 where

import Data.List

import Base
import Rising
import Juke

solutionFrom [] = solutionFrom ["4", "1000000"]
solutionFrom [sS, maxnS] = return $ show $ solutionGen (read sS) (read maxnS)

solutionGen s maxn =
	uniquePrimeFactorsCountMap maxn
	|> tails
	|> filter good
	|> head
	|> head
	|> fst
	where
	pattern = genericReplicate s s
	good k = (map snd $ genericTake s k) == pattern

uniquePrimeFactorsCountMap maxn =
	zip [1..maxn] $ map sum $ transpose $ map expandedMultiplesOf $ smallPrimes
	where
	smallPrimes = takeWhile (\ p -> p * p < maxn) primes''
	expandedMultiplesOf n = genericTake maxn $ cycle $ genericReplicate (n-1) 0 ++ [1]
