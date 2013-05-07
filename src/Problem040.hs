--------------------------------------------------------------------------------
--  Problem 40
--------------------------------------------------------------------------------
--  210
--------------------------------------------------------------------------------

module Problem040 where

import Base
import Juke

solutionFrom [] = solution

solution = product $ take 7 $ ds

ds = map (digits !!) $ map (subtract 1) $ map (10 ^) $ [0 ..]

digits = concat $ map (itol 10) $ [1 ..]
