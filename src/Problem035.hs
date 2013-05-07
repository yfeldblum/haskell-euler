--------------------------------------------------------------------------------
--  Problem 35
--------------------------------------------------------------------------------
--  Let a prime n be called a circular prime if all rotations of its digits are
--  themselves prime. How many circular primes are there below one million?
--------------------------------------------------------------------------------
--  55
--------------------------------------------------------------------------------

module Problem035 where

import Data.List (inits, tails)

import Juke (primes', isPrime, itol, ltoi)

solutionFrom [] = solutionFrom ["10", "1000000"]
solutionFrom [baseS, maxnS] = solutionGen (read baseS) (read maxnS)

solutionGen base maxn = fromIntegral $ length $ filter (good base) $ takeWhile (< maxn) $ primes'

good base n = and' $ map (isPrime primes') $ digitRotations base $ n

digitRotations base n = map (ltoi base) $ rotations $ itol base $ n

rotations xs = init $ zipWith (flip (++)) (inits xs) (tails xs)

or' xs = not $ null $ filter id $ xs
and' xs = null $ filter not $ xs
