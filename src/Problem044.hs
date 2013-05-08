--------------------------------------------------------------------------------
--  Problem 44
--------------------------------------------------------------------------------
--  5482660
--------------------------------------------------------------------------------

module Problem044 where

import Base
import Rising
import Juke

solutionFrom [] = return $ show $ solution

solution = (\ (p1, p2, s, d) -> d) $ head solutions

solutions =
	[ (p1, p2, s, d)
	| n <- [2 ..]
	, let p1 = pentagonal n
	, p2 <- map pentagonal $ [n - 1, n - 2 .. 1]
	, let s = p1 + p2
	, let d = p1 - p2
	, isPentagonal s
	, isPentagonal d
	]

pentagonals = map pentagonal $ [1 ..]

pentagonal n = (n * (3 * n - 1)) `quot` 2

pentagonal' :: (Integral p, Floating n) => p -> n
pentagonal' p = (1 + sqrt(1 + 24 * (fromIntegral p))) / 6

isPentagonal :: (Integral p) => p -> Bool
isPentagonal p = d < (1e-10)
	where
		n :: (Floating n) => n
		n = pentagonal' p
		d :: (Floating n) => n
		d = n - (fromIntegral $ floor n)
