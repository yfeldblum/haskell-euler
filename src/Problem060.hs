--------------------------------------------------------------------------------
--  Problem 60
--------------------------------------------------------------------------------
--  Find the lowest sum for a set of five primes for which any two primes in the
--  set concatenate to produce another prime.
--------------------------------------------------------------------------------
--  26033
--------------------------------------------------------------------------------

module Problem060 where

import qualified Juke ( primes'', isPrime )
import Juke ( itol, ltoi )

solutionFrom [] = solutionFrom ["10", "5"]
solutionFrom [baseS, sizeS] = solutionGen (read baseS) (read sizeS)

primes = Juke.primes''
isPrime = Juke.isPrime primes

solutionGen base size = sum $ head $ primesSets base size

primesSets base 1 = map (: []) primes
primesSets base size = concatMap (extendPrimesSet base) $ primesSets base (size - 1)

extendPrimesSet base ps =
	let minp = head ps in
	[ p : ps
	| p <- takeWhile (< minp) primes
	, isExtensionOfPrimesSet base p ps
	]

isExtensionOfPrimesSet base p ps = all (worksWith p) ps where
	worksWith p p' = isPrime (concatDigitsOf base p p') && isPrime (concatDigitsOf base p' p)

concatDigitsOf base a b = ltoi base $ concat $ map (itol base) $ [a, b]
