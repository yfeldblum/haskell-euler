--------------------------------------------------------------------------------
--  Problem 65
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  272
--------------------------------------------------------------------------------

module Problem065 where

import Data.Ratio

import Base ( (|>) )
import Juke ( itol )

solutionFrom [] = return $ show $ solution

solution = sum $ itol 10 $ numerator $ converge eValues 100

eValues = (2, ks 1) where
	ks n = 1 : (2 * n) : 1 : ks (n + 1)

sqrt2Values = (1, repeat 2)

converge values n = foldr1 (+%) $ take n $ map (% 1) $ v' : vs'
	where
	(v', vs') = values

a +% b = a + denominator b % numerator b
infixr +%
