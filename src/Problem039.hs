--------------------------------------------------------------------------------
--  Problem 39
--------------------------------------------------------------------------------
--  For which value of p < 1000 is maximized the number of distinct Pythagorean
--  triples having sum p?
--------------------------------------------------------------------------------
--  840
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Problem039 where

import Base

solutionFrom [] = return $ show $ solution

newtype K a = K a deriving (Eq)
instance Wrapped K where unwrap (K a) = a
instance (Eq a, Eq b, Ord b) => Ord (K (a, b)) where
	compare (K (a, b)) (K (a', b')) = compare b b'

solution = fst $ unwrap $ maximum $ map K $ map k $ filter even $ [2, 4 .. 999]

k p = (p, length $ triples p)

triples p =
	[ (a, b, c)
	| c <- [1 .. p `div` 2]
	, a <- [1 .. c - 1]
	, let b = p - (c + a)
	, a < b
	, a^2 + b^2 == c^2
	]
