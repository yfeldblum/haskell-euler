--------------------------------------------------------------------------------
--  Problem 64
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  1322
--------------------------------------------------------------------------------

module Problem064 where

import Data.Ratio

import Base ( (|>) )
import Juke ( isqrt )

solutionFrom [] = solutionFrom ["10000"]
solutionFrom [maxS] = return $ show $ solutionGen (read maxS)

solutionGen max =
	[1..max]
	|>	filter (\ n -> let n' = isqrt n in n /= n' * n')
	|>	map expansion
	|>	map snd
	|>	map findMinCycle
	|>	filter (odd . length)
	|>	length

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

findMinCycle as = head $
	[ a
	| l <- [1..]
	, let (a, b) = splitAt l as
	, a == take l b
	]
