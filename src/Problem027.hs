--------------------------------------------------------------------------------
--  Problem 27
--------------------------------------------------------------------------------
--  Considering quadratic function of the form p(n) = n^2 + an + b, where
--  |a| < 1000 and |b| < 1000, find the product of the coefficients a and b for
--  the quadratic function p(n) that produces the maximum number of primes for
--  consecutive values of n, starting with n = 0. That is, find the a and b that
--  maximize (length $ takeWhile isPrime $ map p $ [0 ..]).
--------------------------------------------------------------------------------
--  -59231
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Problem027 where

import Data.Ix (range)
import Data.List (foldl1')

import Juke (primes')
import Base

solutionFrom [] = solutionFrom ["1000", "1000"]
solutionFrom [maxaS, maxbS] = solutionGen (read maxaS) (read maxbS)

newtype K a = K a deriving (Eq)
instance Wrapped K where unwrap (K a) = a
instance (Eq a, Eq b, Ord b) => Ord (K (a, b)) where
	compare (K (a, b)) (K (a', b')) = compare b b'

solutionGen maxa maxb = k $ unwrap $ maximum' $ map K $ [ (i, f i) | i <- range b ] where
	b = ((-a, -b), (a, b)) where
		(a, b) = (maxa - 1, maxb - 1)
	f (a, b) = length $ takeWhile isPrime $ p' a b
	k ((a, b), _) = a * b

p a b n = n * n + a * n + b

p' a b = map (p a b) $ [0 ..]

isPrime = (`elemS` primes')

--n `elemS` xs = not $ null $ takeWhile (== EQ) $ dropWhile (== GT) $ map (compare n) $ xs where

elemS n [] = False
elemS n (x:xs) =
	case compare n x of
		LT -> False
		EQ -> True
		GT -> elemS n xs
