--------------------------------------------------------------------------------
--  Problem 18
--------------------------------------------------------------------------------
--  Find the maximum total from top to bottom of the triangle (given).
--------------------------------------------------------------------------------
--  1074
--------------------------------------------------------------------------------

module Problem018 where

import Data.List (foldr1)
import System.IO.Unsafe (unsafePerformIO)

solutionFrom [] = solutionFrom ["data/Problem018.data"]
solutionFrom [filenameS] = return $ show $ solution filenameS

triangle filename = map (map read) $ map words $ lines $
	unsafePerformIO $ readFile filename

reduce a b = zipWith (+) a b' where
	b' = zipWith max b (tail b)

solution filename = head $ foldr1 reduce $ triangle filename
