--------------------------------------------------------------------------------
--  Problem 56
--------------------------------------------------------------------------------
--  Considering natural numbers of the form a^b, where a,b < 100, what is the
--  maximal digital sum?
--------------------------------------------------------------------------------
--  972
--------------------------------------------------------------------------------

module Problem056 where

import Juke ( itol )

solutionFrom [] = solutionFrom ["10", "100"]
solutionFrom [baseS, maxS] = return $ show $ solutionGen (read baseS) (read maxS)

solutionGen base max = maximum $
	[ sum $ itol base c
	| a <- [1..(max-1)]
	, b <- [1..(max-1)]
	, let c = a ^ b
	]
