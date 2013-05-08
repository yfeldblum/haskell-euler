--------------------------------------------------------------------------------
--  Problem 31
--------------------------------------------------------------------------------
--  In England the currency is made up of pound and pence, and there are eight
--  coins in general circulation: 1p, 2p, 5p, 10p, 20p, 50p, 1lb, 2lb. How many
--  different ways can 2lb be made using any number of coins?
--------------------------------------------------------------------------------
--  73682
--------------------------------------------------------------------------------

module Problem031 where

import Data.List (nub)

solutionFrom [] = return $ show $ solution

solution = length $ foldl replacements' [coins] $ tail values

replacements' coinsl value = concat $ map (flip replacements value) $ coinsl

replacements coins k = map (replace coins k) $ [0 .. q] where
	q = (`quot` k) $ snd $ head $ coins
replace coins k n = add n k $ add (k * (-n)) 1 $ coins
add n k [] = []
add n k ((v, c) : xs) =
	if k == v
	then (v, c + n) : xs
	else (v, c) : add n k xs

values = [1, 2, 5, 10, 20, 50, 100, 200]
counts = 200 : replicate (length values - 1) 0
coins = zip values counts
