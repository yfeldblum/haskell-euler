﻿--------------------------------------------------------------------------------
--  Problem 48
--------------------------------------------------------------------------------
--  9110846700
--------------------------------------------------------------------------------

module Problem048 where

import Base
import Rising
import Juke

solutionFrom [] = return $ show $ solution

solution = (`rem` (10^10)) $ sum $ map (\ n -> n^n) $ [1 .. 1000]
