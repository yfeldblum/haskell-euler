--------------------------------------------------------------------------------
--  Problem 36
--------------------------------------------------------------------------------
--  Find the sum of all numbers, less than one million, which are palindromes in
--  base 10 and base 2.
--------------------------------------------------------------------------------
--  872187
--------------------------------------------------------------------------------

module Problem036 where

import Juke (itol)

solutionFrom [] = solutionFrom ["2", "10", "1000000"]
solutionFrom args = return $ show $ solutionGen (map read (init args)) (read (last args))

solutionGen bases maxn = sum $ filter (good bases) $ [1 .. maxn - 1]

good bases n = and $ map (flip pal n) $ bases

pal base n = let l = itol base n in l == reverse l
