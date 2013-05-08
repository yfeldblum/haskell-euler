--------------------------------------------------------------------------------
--  Problem 34
--------------------------------------------------------------------------------
--  Find the sum of all numbers (excluding 0, 1, 2) which are equal to the sum
--  of the factorials of their digits.
--------------------------------------------------------------------------------
--  40730
--------------------------------------------------------------------------------

module Problem034 where

import Data.List (foldl')
import Data.Array ((!), array, range)

import Juke (factorial, itol)

solutionFrom [] = return $ show $ solution

solution = sum' $ filter good $ [3 .. 2540160]

good n = (n ==) $ sum' $ map factorial' $ itol 10 $ n

sum' = foldl' (+) 0

factorial' = (factorials !) where
	bounds = (0, 9)
	factorials = array bounds [ (i, factorial i) | i <- range bounds ]
