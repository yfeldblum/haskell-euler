--------------------------------------------------------------------------------
--  Problem 57
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  153
--------------------------------------------------------------------------------

module Problem057 where

import Data.Ratio

import Base ( (|>) )
import Juke ( itol )

solutionFrom [] = solutionFrom ["10", "1000"]
solutionFrom [baseS, itersS] = return $ show $ solutionGen (read baseS) (read itersS)

solutionGen base iters =
	(2%1)
	|>	iterate (\ n -> 2%1 + recip n) 
	|>	map (\ n -> n - 1%1)
	|>	tail
	|>	take iters
	|>	filter (\ n -> (length $ itol base $ numerator $ n) > (length $ itol base $ denominator $ n))
	|>	length
