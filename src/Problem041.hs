--------------------------------------------------------------------------------
--  Problem 41
--------------------------------------------------------------------------------
--  What is the largest n-digit pandigital prime? An n-digit pandigital is a
--  number which, when written out in base 10, is an n-digit number making use
--  of the digits 1 to n, each digit exactly once.
--------------------------------------------------------------------------------
--  7652413
--------------------------------------------------------------------------------

module Problem041 where

import Data.List ( sort )
import Base
import Juke

solutionFrom [] = return $ show $ solution

{-
solution =
	primes''
	|> takeWhile (< (10^10))
	|> filter isPandigital
	|> last
-}

isPandigital n = sort ds == [1 .. length ds]
	where
		ds = itol 10 n

pandigitals n = map (ltoi 10) $ permutations [1 .. n]

solution =
	[1 .. 9]
	|> map pandigitals
	|> concat
	|> filter (isPrime primes')
	|> maximum
