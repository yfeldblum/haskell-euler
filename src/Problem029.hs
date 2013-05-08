--------------------------------------------------------------------------------
--  Problem 29
--------------------------------------------------------------------------------
--  How many distinct terms are generated by a^b for 2 <= a, b <= 100?
--------------------------------------------------------------------------------
--  9183
--------------------------------------------------------------------------------

module Problem029 where

import Data.List (nub)

solutionFrom [] = solutionFrom ["100"]
solutionFrom [maxnS] = return $ show $ solutionGen (read maxnS)

solutionGen maxn = fromIntegral $ length $ nub $ [ a ^ b | a <- [2 .. maxn], b <- [2 .. maxn] ]
