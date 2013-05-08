--------------------------------------------------------------------------------
--  Problem 22
--------------------------------------------------------------------------------
--  What is the total of all the name scores? A name score is the sum of the
--  letter scores in that name multiplied by the index of that name in a sorted
--  list of all the names. The letter value of 'A' is 1, of 'B' is 2, etc.
--------------------------------------------------------------------------------
--  871198282
--------------------------------------------------------------------------------

module Problem022 where

import Data.Char (ord)

import Base
import Sort

solutionFrom [] = solutionFrom ["data/Problem022.data"]
solutionFrom [filenameS] = do
	text <- readFile filenameS
	return $ show $ solution text

solution text = fromIntegral $ sum scores where
	scores = map pairScore $ pairs
	pairScore (n, i) = i * nameScore n
	nameScore n = sum $ map charScore $ n
	charScore c = ord c - ord 'A' + 1
	pairs = zip (quicksort names) ([1 .. ])
	names = map (filter (/= '"')) $ splitOn ',' $ text
