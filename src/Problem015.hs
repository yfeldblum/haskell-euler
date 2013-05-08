--------------------------------------------------------------------------------
--  Problem 15
--------------------------------------------------------------------------------
--  How many routes are there, starting in the top left corner and proceeding
--  without backtracking, through a 20x20 grid?
--------------------------------------------------------------------------------
--  137846528820
--------------------------------------------------------------------------------

module Problem015 where

import Base (dp)

solutionFrom [] = solutionFrom ["20", "20"]
solutionFrom [wS, hS] = return $ show $ solutionGen (read wS) (read hS)

solutionGen w h = dp ((0, 0), (w, h)) sol (w, h)

sol rec (0, n) = 1
sol rec (n, 0) = 1
sol rec (w, h) | h < w     = rec (h, w)
sol rec (w, h) | otherwise = rec (w - 1, h) + rec (w, h - 1)
