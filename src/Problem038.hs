--------------------------------------------------------------------------------
--  Problem 38
--------------------------------------------------------------------------------
--  932718564
--------------------------------------------------------------------------------

module Problem038 where

import Data.List( sort )
import Base( (|>) )
import Juke( itol, ltoi )

solutionFrom [] = solution

solution = maximum pandigitals

pandigitals = concat $ map pandigitalsForN $ [1 .. 9]

pandigitalsForN n =
	[1 .. 10000]
	|> map (\m -> pandigitalForMAndN m n)
	|> justs
	where
		justs [] = []
		justs (Nothing : xs) = justs xs
		justs (Just (x) : xs) = x : justs xs

pandigitalForMAndN m n =
	if length digits /= 9 then Nothing else
	if sort digits /= [1 .. 9] then Nothing else
	Just (ltoi 10 digits)
	where
		digits =
			[1 .. n]
			|> map (m *)
			|> map (itol 10)
			|> concat
