--------------------------------------------------------------------------------
--  Problem 67
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  7273
--------------------------------------------------------------------------------

module Problem067 where

import Base ( unwrap )

solutionFrom [] = solutionFrom ["data/Problem067.triangle.txt"]
solutionFrom [filenameS] = solution $ parseTriangle $ unwrap $ readFile $ filenameS

parseTriangle = map (map read) . map words . lines

solution triangle = head $ foldr1 sums $ triangle

sums xs ys = zipWith (+) xs (maxes ys)

maxes xs = zipWith max xs (tail xs)
