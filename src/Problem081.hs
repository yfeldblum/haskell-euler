--------------------------------------------------------------------------------
--  Problem 81
--------------------------------------------------------------------------------
--  Find the minimal path sum of the matrix in the associated matrix file, where
--  a path is a sequence of positions either immediately rightward or downward
--  of a previous position and a path sum is the sum of the numbers across the
--  positions in the path.
--------------------------------------------------------------------------------
--  427337
--------------------------------------------------------------------------------

module Problem081 where

import Debug.Trace ( traceShow )
import Data.Ix ( range )
import Data.Array ( array, (!) )

import Base ( splitOn, dp )

solutionFrom [] = solutionFrom ["data/Problem081.matrix.txt"]
solutionFrom [filenameS] = do
	text <- readFile filenameS
	return $ show $ solution text

readGrid :: (Integral a, Read a) => String -> [[a]]
readGrid text = map (map read) $ map (splitOn ',') $ lines text

solution text = finalNumber
	where
	pair k = (k, k)
	gridSize = fromIntegral $ length grid
	gridBounds = (pair 0, pair (gridSize-1))
	grid = readGrid text
	grid' = array gridBounds [ (idx, grid !! i !! j) | idx@(i,j) <- range gridBounds ]
	gridSumsRec' = dp gridBounds compute' where
		compute' rec (0,0) = grid' ! (0,0)
		compute' rec (0,j) = grid' ! (0,j) + rec (0,j-1)
		compute' rec (i,0) = grid' ! (i,0) + rec (i-1,0)
		compute' rec (i,j) = grid' ! (i,j) + min (rec (i,j-1)) (rec (i-1,j))
	gridSums' = array gridBounds [ (idx, gridSumsRec' idx) | idx <- range gridBounds ]
	finalNumber = gridSums' ! pair (gridSize-1)
