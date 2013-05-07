--------------------------------------------------------------------------------
--  Problem 46
--------------------------------------------------------------------------------
--  5777
--------------------------------------------------------------------------------

module Problem046 where

import Base
import Rising
import Juke

solutionFrom [] = solution

solution = head solutions

solutions = filter failsTheConjecture composites

composites = Rising.exclude [3, 5..] primes'

failsTheConjecture n =
	[1 ..]
	|> map (\ k -> n - 2 * k * k)
	|> takeWhile (> 0)
	|> filter (isPrime' primes')
	|> null
