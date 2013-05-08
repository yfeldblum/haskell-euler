--------------------------------------------------------------------------------
--  Problem 18
--------------------------------------------------------------------------------
--  Find the maximum total from top to bottom of the triangle (given).
--------------------------------------------------------------------------------
--  1074
--------------------------------------------------------------------------------

module Problem018 where

import Data.List (foldr1)

solutionFrom [] = solutionFrom ["data/Problem018.data"]
solutionFrom [filenameS] = do
	text <- readFile filenameS
	return $ show $ solution text

triangle text = map (map read) $ map words $ lines $ text

reduce a b = zipWith (+) a b' where
	b' = zipWith max b (tail b)

solution text = head $ foldr1 reduce $ triangle text
