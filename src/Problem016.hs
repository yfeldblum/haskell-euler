--------------------------------------------------------------------------------
--  Problem 16
--------------------------------------------------------------------------------
--  What is the sum of the digits of the number 2^1000?
--------------------------------------------------------------------------------
--  1366
--------------------------------------------------------------------------------

module Problem016 where

import Data.Char (digitToInt)

solutionFrom [] = solutionFrom ["2", "1000"]
solutionFrom [baseS, expS] = return $ show $ solutionGen (read baseS) (read expS)

solutionGen base exp = fromIntegral $ sum $ map digitToInt $ show $ base^exp
