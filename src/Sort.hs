module Sort (
	sorted,
	quicksort,
	mergesort
) where

import Data.List (partition)

import Base (unmerge)
import qualified Rising

-- sorted

sorted [] = True
sorted [x] = True
sorted (x : xt) = x <= head xt && sorted xt

-- quicksort

quicksort [] = []
quicksort (x : xs) = quicksort ys ++ x : quicksort zs where
	(ys, zs) = partition (< x) xs

--  mergesort

mergesort [] = []
mergesort [x] = [x]
mergesort xs = Rising.merge (mergesort ys) (mergesort zs) where
	(ys, zs) = unmerge xs
