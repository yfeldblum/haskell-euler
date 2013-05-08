--------------------------------------------------------------------------------
--  Problem 50
--------------------------------------------------------------------------------
--  Which prime, below one-million, can be written as the sum of the most
--  consecutive primes?
--------------------------------------------------------------------------------
--  997651
--------------------------------------------------------------------------------

module Problem050 where

import Data.List ( maximumBy )
import Base
import Sort
import Rising
import Juke

solutionFrom [] = return $ show $ solution'

solution = mks primes''

solution' =
	[numPrimes, numPrimes - 1 .. 1]
	|> map (\ n -> sums n primes'')
	|> map (takeWhile (< 1000000))
	|> map (filter (isPrime' primes''))
	|> filter (not . null)
	|> map head
	|> head

numPrimes = length $ takeWhile (< 1000000) $ primes''

sums n primes = (sum $ take n $ primes) : sums n (tail primes)

mks ps =
	ps
	|> takeWhile (< 1000000)
	|> ks
	|> maximumBy (\ (an, ap) (bn, bp) -> compare an bn)
	|> snd

ks [] = []
ks ps = k ps : ks (tail ps)

k primes =
	primes
	|> scanl1 (+)
	|> takeWhile (< 1000000)
	|> zip [1 ..]
	|> filter (isPrime' primes . snd)
	|> last
