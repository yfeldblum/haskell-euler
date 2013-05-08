--------------------------------------------------------------------------------
--  Problem 28
--------------------------------------------------------------------------------
--  Starting with the number 1 and moving to the right in a clockwise direction
--  a spiral is formed. What is the sum of both diagonals in a 1001 by 1001
--  spiral formed in this way?
--------------------------------------------------------------------------------
--  669171001
--------------------------------------------------------------------------------

module Problem028 where

import Data.List (foldl')

solutionFrom [] = solutionFrom ["1001"]
solutionFrom [xS] = return $ show $ solutionGen (read xS)

fStep = doubleUp [4, 8 ..] where
	doubleUp [] = []
	doubleUp (x : xs) = x : x : doubleUp xs

bStep = [2, 4 ..]

solutionGen x = a + b - 1 where
	a = foldl' (+) 0 $ take x $ scanl (+) 1 $ fStep
	b = foldl' (+) 0 $ take x $ scanl (+) 1 $ bStep
