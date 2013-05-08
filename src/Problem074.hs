--------------------------------------------------------------------------------
--  Problem 74
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  402
--------------------------------------------------------------------------------

module Problem074 where

import Data.List ( sort )
import qualified Data.Map as Map

import Base ( (|>), on, dp )
import Juke ( itol, factorial )
import qualified Rising ( elem )

solutionFrom [] = return $ show $ solution

base = 10

solution = fromIntegral $ length $ filter ((== 60) . snd) $ chainLengths' baseChainLengths' $ [1..1000000]

f = sum . map factorial' . itol base where
	factorial' = dp (0, base) $ const factorial

chain n = as ++ uncycle bs where
	(as, bs) = break isLooper $ iterate f $ n

chainLengths' memo [] = []
chainLengths' memo (x:xs) =
	let (memo', cx) = chainLength' memo x in
	(x, cx) : chainLengths' memo' xs

chainLength' memo n =
	case Map.lookup n memo of
		Just cn -> (memo, cn)
		Nothing ->
			let fn = f n in
			let (memo', cn') = chainLength' memo fn in
			let cn'' = cn' + 1 in
			let memo'' = Map.insert n cn'' memo' in
			(memo'', cn'')

baseChainLengths' = go Map.empty loopers where
	go m [] = m
	go m (l:ls) = go (Map.insert l (flen l) m) ls
	flen = length . uncycle . iterate f

isLooper = flip Rising.elem loopers
loopers = sort $ concat $ map uncycle $ map (iterate f) $ [1, 2, 145, 169, 871, 872] ++ [40585]

uncycle (x:xs) = x : go xs where
	go (y:ys) | y == x = []
	go (y:ys) | otherwise = y : go ys
