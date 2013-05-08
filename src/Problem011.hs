--------------------------------------------------------------------------------
--  Problem 11
--------------------------------------------------------------------------------
--  What is the greatest product of four numbers in any direction (up, down,
--  left, right, or diagonally) in the 20x20 grid (given)?
--------------------------------------------------------------------------------
--  70600674
--------------------------------------------------------------------------------

module Problem011 where

import Data.Ix
import Data.Array
import System.IO.Unsafe

solutionFrom [] = solutionFrom ["data/Problem011.data"]
solutionFrom [filenameS] = do
	text <- readFile filenameS
	return $ show $ solution $ readGrid $ text

solution nums = maximum $ [horiz, verti, fdiag, bdiag] where
	horiz = maximum $ map product $ map (map numgen) $ map horizgen $ range ((0, 0), (19, 16))
	verti = maximum $ map product $ map (map numgen) $ map vertigen $ range ((0, 0), (16, 19))
	fdiag = maximum $ map product $ map (map numgen) $ map fdiaggen $ range ((0, 3), (16, 19))
	bdiag = maximum $ map product $ map (map numgen) $ map bdiaggen $ range ((3, 0), (19, 16))
	horizgen (i, j) = map (\ k -> (i, j + k)) $ [0 .. 3]
	vertigen (i, j) = map (\ k -> (i + k, j)) $ [0 .. 3]
	fdiaggen (i, j) = map (\ k -> (i + k, j - k)) $ [0 .. 3]
	bdiaggen (i, j) = map (\ k -> (i - k, j + k)) $ [0 .. 3]
	numgen (i, j) = nums !! i !! j
	pair k = (k, k)
	arrayb = (pair 0, pair 19)
	narray = array arrayb [ (idx, numgen idx) | idx <- range arrayb ]

readGrid text = map (map read) $ map words $ lines $ text
