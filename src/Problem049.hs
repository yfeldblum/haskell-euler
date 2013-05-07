--------------------------------------------------------------------------------
--  Problem 49
--------------------------------------------------------------------------------
--  296962999629
--------------------------------------------------------------------------------

module Problem049 where

import Data.List ( sort )
import Base
import Sort
import Rising
import Juke

solutionFrom [] = solution

solution = sequences !! 1

sequences =
	[ ltoi 10 $ concat $ map (itol 10) $ [n1, n2, n3]
	| n <- [1000 .. 9999]
	, d <- [1 .. 4500]
	, let (n1, n2, n3) = (n, n + d, n + d + d)
	, let ps = nub $ sort $ map (ltoi 10) $ permutations $ itol 10 $ n
	, Rising.elem n2 ps
	, Rising.elem n3 ps
	, isPrime' primes' n1
	, isPrime' primes' n2
	, isPrime' primes' n3
	]
