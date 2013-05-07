--------------------------------------------------------------------------------
--  Problem 4
--------------------------------------------------------------------------------
--  Find the largest palindrome made from the product of two 3-digit numbers.
--  [Note: in base 10.]
--------------------------------------------------------------------------------
--  906609
--------------------------------------------------------------------------------

module Problem004 where

import Data.Maybe (catMaybes)

solutionFrom [] = solutionFrom ["10"]
solutionFrom [baseS] = solutionGen (read baseS)

solutionGen base = head $ filter isLegal $ palindromes where
	palindromes = [ fromList [a, b, c, c, b, a] | (a, b, c) <- triples ] where
		triples = [ (a, b, c) | a <- digits, b <- digits, c <- digits ]
		digits = reverse [0 .. base - 1]
		fromList = foldl f 0 where
			f i c = base * i + c
	isLegal n = not $ null $ filter isLegalPair $ catMaybes $ map pair $ nums where
		isLegalPair (k, q) = inrange k && inrange q
		pair k = let (q, r) = quotRem n k in if r /= 0 then Nothing else Just (k, q)
		(minn, maxn, nums) = (base ^ 2, base ^ 3, [minn .. maxn - 1])
		inrange k = not (k < minn) && k < maxn
