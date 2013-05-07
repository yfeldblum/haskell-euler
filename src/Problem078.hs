--------------------------------------------------------------------------------
--  Problem 78
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  55374
--------------------------------------------------------------------------------

module Problem078 where

import Base ( (|>), y, dp )
import PartitionFunction ( p' )

solutionFrom [] = solutionFrom ["1000000"]
solutionFrom [nS] = solutionGen (read nS)

solutionGen n = [1 .. maxk] |> filter good |> head
	where
	good k = p k `mod` n == 0
	p = dp (0, maxk) p'
	maxk = n `div` 10
