--------------------------------------------------------------------------------
--  Problem 63
--------------------------------------------------------------------------------
--  How many n-digit positive integers exist which are also n-th powers?
--------------------------------------------------------------------------------
--  49
--------------------------------------------------------------------------------

module Problem063 where

import Data.List ( nub, sort )

import Juke ( itol )

solutionFrom [] = solutionFrom ["10"]
solutionFrom [baseS] = return $ show $ solutionGen (read baseS)

solutionGen base = length $ nub $ sort $ pairs base

pairs base =
	[ n ^ p
	| (n, ps) <- chain base
	, p <- ps
	]

chain base = takeWhile (\ (n, ps) -> not $ null ps) $ map (\ n -> (n, powersForNumber base n)) $ [1..(base-1)]

powersForNumber base n = map fst $ takeWhile (\ (p, vll) -> vll == p) $ dropWhile (\ (p, vll) -> vll > p) $ powers
	where
	powers =
		[ (p, vll)
		| p <- [1..]
		, let v = n ^ p
		, let vl = itol base v
		, let vll = length vl
		]
