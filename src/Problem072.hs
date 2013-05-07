--------------------------------------------------------------------------------
--  Problem 72
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  303963552391
--------------------------------------------------------------------------------

module Problem072 where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List ( minimumBy, foldl', sort )
import Data.Ratio

import Base ( (|>), on, dp )
import Juke ( itol )

solutionFrom [] = solutionFrom ["1000000"]
solutionFrom [maxbS] = solutionGen (read maxbS)

solutionGen maxb =
	[2..maxb]
	|>	map totient
	|>	sum
	where
	totient = totientd maxb

totientd maxb = dp (1, maxb) t
	where
	divs = makeLeastDivisorsArray maxb
	t f 1 = 1
	t f n =
		let (p, q) = (divs ! n, n `quot` p) in
		if q == 1 then p - 1 else
		let pexp = last $ takeWhile (\ s -> n `rem` (p ^ s) == 0) $ [1..] in
		let (ps, qs) = (p ^ pexp, n `quot` ps) in
		if qs == 1 then (p - 1) * p ^ (pexp - 1) else
		f ps * f qs

makeLeastDivisorsArray maxb =
	runSTArray $ do
		arr <- newArray (1, maxb) 0 :: ST s (STArray s Integer Integer)
		writeArray arr 1 1
		forM_ [2..maxb] $ \ i -> do
			d <- readArray arr i
			when (d == 0) $ do
				forM_ [i, 2 * i .. maxb] $ \ j -> do
					--s <- readArray arr j
					--when (s == 0) $ do
						writeArray arr j i
		return arr
