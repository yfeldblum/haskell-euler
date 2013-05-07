--------------------------------------------------------------------------------
--  Problem 70
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  8319823
--------------------------------------------------------------------------------

module Problem070 where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List ( minimumBy, foldl', sort )
import Data.Ratio

import Base ( (|>), on, dp )
import Juke ( itol )

solutionFrom [] = solutionFrom ["10000000"]
solutionFrom [maxbS] = solutionGen (read maxbS)

solutionGen maxb =
	[2..(maxb-1)]
	|>	map (\ n -> (n, totient n))
	|>	filter (\ (n, tn) -> n `permeq` tn)
	|>	map (\ (n, tn) -> (n, n // tn))
	|>	minimumBy (compare `on` snd)
	|>	fst
	where
	totient = totientd maxb
	(//) = (/) `on` fromIntegral
	permeq = (==) `on` (sort . itol 10)

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
