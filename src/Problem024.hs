--------------------------------------------------------------------------------
--  Problem 24
--------------------------------------------------------------------------------
--  What is the millionth permutation of the digits 0 through 9, sorted in lexi-
--  cographical order?
--------------------------------------------------------------------------------
--  2783915460
--------------------------------------------------------------------------------

module Problem024 where

import Juke (permutations)

solutionFrom [] = solutionFrom ["10", "1000000"]
solutionFrom [nS, kS] = return $ show $ solutionGen (read nS) (read kS)

solutionGen :: Int -> Int -> Integer
solutionGen n k = read $ concat $ map show $ (!! (k - 1)) $ permutations $ [0 .. n - 1]
