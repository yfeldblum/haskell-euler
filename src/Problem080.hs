--------------------------------------------------------------------------------
--  Problem 80
--------------------------------------------------------------------------------
--  For the first one hundred natural numbers, find the total of the digital
--  sums of the first one hundred decimal digits for all the irrational square
--  roots. The digital sum is the sum-of-digits.
--------------------------------------------------------------------------------
--  40886
--------------------------------------------------------------------------------

module Problem080 where

import Data.Maybe ( fromJust, isJust )

import Base ( (|>) )
import Juke ( itol, isqrt )

solutionFrom [] = return $ show $ solution

solution = [1..100] |> map krasket |> justs |> sum

krasket n = krasket' (n * 10^200) where
	krasket' nn | perfectSquare nn = Nothing
	krasket' nn | otherwise = Just $ sum $ take 100 $ itol 10 $ isqrt $ nn

perfectSquare n = r^2 == n where
	r = isqrt n

justs = map fromJust . filter isJust
