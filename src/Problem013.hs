--------------------------------------------------------------------------------
--  Problem 13
--------------------------------------------------------------------------------
--  Work out the first ten digits of the sum of the following one-hundred 50-
--  digit numbers (given).
--------------------------------------------------------------------------------
--  5537376230
--------------------------------------------------------------------------------

module Problem013 where

import System.IO.Unsafe

solutionFrom [] = solutionFrom ["data/Problem013.data"]
solutionFrom [filenameS] = solution $ filenameS

solution filename = read $ take 10 $ show $ sum $ map read $ lines $ unsafePerformIO $ readFile $ filename
