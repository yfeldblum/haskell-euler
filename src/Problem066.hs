--------------------------------------------------------------------------------
--  Problem 66
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  661
--------------------------------------------------------------------------------

module Problem066 where

import Data.Ratio
import Data.List ( maximumBy )

import Base ( (|>), on )
import Juke ( isqrt )

solutionFrom [] = return $ show $ solution

solution = fst $ maximumBy (compare `on` snd) $ solutions 1000

solutions max =
	[ (d, x)
	| d <- [1..max]
	, let sqrtd = isqrt d
	, d /= sqrtd * sqrtd
	, let x = solutionFor d
	]

solutionFor d = head $
	[ x
	| let cf = continuedFractionForSqrtOf d
	, c <- convergents cf
	, let (x, y) = (numerator c, denominator c)
	, x * x - d * y * y == 1
	]

continuedFractionForSqrtOf c =
	let sqrtc = isqrt c in
	if sqrtc * sqrtc == c then error ("c is a square: " ++ show c) else
	expansion c

convergents cf =
	[ res
	| l <- [1..]
	, let res = foldr1 (+%) $ take l $ cf'
	]
	where
	k = \ (a,_,_,_) -> a
	cf' = map (% 1) $ map k $ fst cf : snd cf
	a +% b = a + denominator b % numerator b

step (a, b, c, d) =
	let m = isqrt c in
	let b' = denominator $ b % (c - d * d) in
	let (a', r) = (m + d) `quotRem` b' in
	let d' = m - r in
	(a', b', c, d')

prep c =
	let m = isqrt c in
	(m, 1, c, m)

rep c = iterate step (prep c)

expansion c =
	let a:as = rep c in
	(a, as)

