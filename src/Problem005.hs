--------------------------------------------------------------------------------
--  Problem 5
--------------------------------------------------------------------------------
--  What is the smallest number that is evenly divisible by all of the numbers
--  from 1 to 20?
--------------------------------------------------------------------------------
--  232792560
--------------------------------------------------------------------------------

module Problem005 where

solutionFrom [] = solutionFrom ["20"]
solutionFrom [maxnS] = solutionGen (read maxnS)

solutionGen maxn = foldl1 lcm [1 .. maxn]
