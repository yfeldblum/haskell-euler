--------------------------------------------------------------------------------
--  Problem 3
--------------------------------------------------------------------------------
--  What is the largest prime factor of the number 600851475143?
--------------------------------------------------------------------------------
--  6857
--------------------------------------------------------------------------------

module Problem003 where

import Juke (primes, primeFactors, divides)

solutionFrom [] = solutionFrom ["600851475143"]
solutionFrom [nS] = return $ show $ solutionFast (read nS)

solutionSlow n = last $ filter (`divides` n) $ takeWhile (< n) $ primes

solutionFast n = last $ primeFactors n
