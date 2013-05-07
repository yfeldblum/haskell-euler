--------------------------------------------------------------------------------
--  Problem 62
--------------------------------------------------------------------------------
--  Find the smallest cube for which exactly five permutations of its digits are
--  cube.
--------------------------------------------------------------------------------
--  127035954683
--------------------------------------------------------------------------------

module Problem062 where

import Data.List ( sort, groupBy, sortBy )

import Base ( (|>), on )
import Juke ( itol )

solutionFrom [] = solution

solution = cuber * cuber * cuber
	where
	ofFivePerms =
		cubesDigitsGroupedSortedGrouped
		|>	filter ((== 5) . length)
	ofFivePermsSmall =
		let k = length $ snd $ head $ head $ ofFivePerms in
		takeWhile ((== k) . length . snd . head) $ ofFivePerms
	cubers = map (map fst) ofFivePermsSmall
	cuber = head $ sort $ concat $ cubers

nums = [0..]
cubes = map (\ x -> x * x * x) nums
cubesDigits = map (sort . itol 10) cubes
cubesDigitsZipped = zip nums cubesDigits
cubesDigitsGrouped = groupBy ((==) `on` (length . snd)) cubesDigitsZipped
cubesDigitsGroupedSorted = map (sortBy (compare `on` snd)) cubesDigitsGrouped
cubesDigitsGroupedSortedGrouped = groupBy ((==) `on` snd) $ concat $ cubesDigitsGroupedSorted
