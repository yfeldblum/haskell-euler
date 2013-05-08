--------------------------------------------------------------------------------
--  Problem 68
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  6531031914842725
--------------------------------------------------------------------------------

module Problem068 where

import Data.List ( sort )

import Juke ( ltoi, permutations )

solutionFrom [] = return $ show $ solution

solution = last $ sort $ map (ltoi 10) $ map expandTen $ map makeString $ patterns

makeString [a,b,c,d,e,f,g,h,i,j] =
	[a,f,g,b,g,h,c,h,i,d,i,j,e,j,f]

expandTen xs = go xs where
	go [] = []
	go (10:xs) = 1 : 0 : go xs
	go (x:xs) = x : go xs

patterns =
	[ rp
	| p' <- permutations [1..9]
	, let p = 10 : p'
	, isPattern p
	, let rp = rotatePattern p
	]

isPattern [a,b,c,d,e,f,g,h,i,j] =
	let s = a + f + g in
	s == b + g + h &&
	s == c + h + i &&
	s == d + i + j &&
	s == e + j + f

rotatePattern p@[a,b,c,d,e,f,g,h,i,j] =
	if a == minimum [a,b,c,d,e] then p else
	rotatePattern [b,c,d,e,a,g,h,i,j,f]
