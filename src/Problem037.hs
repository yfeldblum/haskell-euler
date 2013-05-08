--------------------------------------------------------------------------------
--  Problem 37
--------------------------------------------------------------------------------
--  Find the sum of the only eleven primes that are truncatable from left to
--  right and truncatable from right to left. For example, the prime 3797 is
--  truncatable from left to right because 3797, 797, 97, 7 are all prime, it
--  is also truncatable from right to left because 3797, 379, 37, 3 are all
--  prime.
--------------------------------------------------------------------------------
--  748317
--------------------------------------------------------------------------------

module Problem037 where

import Data.List (tails, inits)

import Juke (primes', itol, ltoi)
import qualified Rising

solutionFrom [] = return $ show $ solution

solution = sum $ take 11 $ dropWhile (< 10) $ truncPrimes 10 $ primes' where

trunc' b n = (k tail n, k init n) where
	k f n = ltoi b $ f $ itol b $ n

truncPrimes b primes = Rising.intersect l r where
	l = truncPrimesL b primes
	r = truncPrimesR b primes

truncPrimesL b primes = tps where
	tps = filter k $ primes
	k n = n < b || Rising.elem (fst $ trunc' b n) tps

truncPrimesR b primes = tps where
	tps = filter k $ primes
	k n = n < b || Rising.elem (snd $ trunc' b n) tps
