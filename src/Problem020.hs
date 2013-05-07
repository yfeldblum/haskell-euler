--------------------------------------------------------------------------------
--  Problem 20
--------------------------------------------------------------------------------
--  Find the sum of the digits in the number 100!
--------------------------------------------------------------------------------
--  648
--------------------------------------------------------------------------------

module Problem020 where

import Data.Char (digitToInt)

import Juke (factorial)

solutionFrom [] = solutionFrom ["100"]
solutionFrom [nS] = solutionGen (read nS)

solutionGen n = fromIntegral $ sum $ map digitToInt $ show $ factorial $ n
