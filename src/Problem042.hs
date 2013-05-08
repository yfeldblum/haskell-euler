--------------------------------------------------------------------------------
--  Problem 42
--------------------------------------------------------------------------------
--  162
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Problem042 where

import Data.Char ( ord )
import Base
import Rising
import Juke

solutionFrom [] = solutionFrom ["data/Problem042.data"]
solutionFrom [filenameS] = return $ show $ solution filenameS

solution filename = length $ filter (isTriangle . getValue) $ theWords filename

theWords filename = map (filter (/= '"')) $ splitOn ',' $ unwrap $ readFile filename

class HasValue a where
	getValue :: (Num n) => a -> n

instance HasValue Char where
	getValue c = fromIntegral $ ord c - ord 'A' + 1

instance HasValue String where
	getValue s = sum $ map getValue $ s

isTriangle n = Rising.elem n triangles

triangles = scanl (+) 0 [1 ..]
