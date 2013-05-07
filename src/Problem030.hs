--------------------------------------------------------------------------------
--  Problem 30
--------------------------------------------------------------------------------
--  Find the sum of all the numbers (excluding 0 and 1) that can be written as
--  the sum of fifth powers of their digits.
--------------------------------------------------------------------------------
--  443839
--------------------------------------------------------------------------------

module Problem030 where

import Data.Char (digitToInt)

import Juke (itol)

solutionFrom [] = solution

solution = sum [ i | i <- [2 .. 1000000], i == f i ] where
	f i = sum $ map (^ 5) $ itol 10 $ i
