--------------------------------------------------------------------------------
--  Problem 51
--------------------------------------------------------------------------------
--  Find the smallest prime which, by replacing part of the number (not neces-
--  sarily adjacent digits) with the same digit, is part of an eight prime value
--  family. A family is a set of numbers of the form AB**C, where A, B, and C
--  are fixed and where replacing all the *'s with the same digit yields a mem-
--  ber of the family.
--------------------------------------------------------------------------------
--  121313
--------------------------------------------------------------------------------

module Problem051 where

import qualified Data.Set as Set

import Base ( (|>) )
import Juke ( primes'' , itol , ltoi )

solutionFrom [] = solutionFrom ["10", "8"]
solutionFrom [baseS, familySizeS] = solutionGen (read baseS) (read familySizeS)

solutionGen base familySize =
	primes
	|>	risingIndex (length . itol base)
	|>	concatMap (\ (size, primesOfSize) -> findPrimesWithFamilyOfFamilySize size primesOfSize)
	|>	head
	where
	primes = Juke.primes''
	digits = [0 .. base - 1]
	findPrimesWithFamilyOfFamilySize size primesOfSize =
		[ prime
		| let primesSet = Set.fromList primesOfSize
		, prime <- primesOfSize
		, pattern <- patterns size
		, let family = familyFor prime pattern primesSet
		, length family == familySize
		, prime `elem` family
		]
	familyFor prime pattern primesSet =
		[ testNumber
		| digit <- digits
		, digit /= 0 || head pattern == False
		, let testNumber = ltoi base $ replacements pattern digit $ itol base $ prime
		, testNumber `Set.member` primesSet
		]

replacements :: Integral a => [Bool] -> a -> [a] -> [a]
replacements pattern digit list = zipWith replaceIf pattern list where
	replaceIf False a = a
	replaceIf True _ = digit

patterns :: Integral a => a -> [[Bool]]
patterns 0 = [ [] ]
patterns len =
	[ val : trailer
	| trailer <- patterns (len - 1)
	, val <- [False, True]
	]

risingIndex :: Integral a => (e -> a) -> [e] -> [(a, [e])]
risingIndex f els = g 0 els where
	g i els = (i, xs) : g (i + 1) ys where
		(xs, ys) = span (\ e -> f e == i) els
