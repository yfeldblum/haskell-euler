module Rising (
	elem,
	intersect,
	union,
	exclude,
	merge,
	nub
) where

import Prelude hiding (elem)
import Data.List (foldl1, foldl1')
import Base

elem a [] = False
elem a (x : xt) =
	case compare a x of
		LT -> False
		EQ -> True
		GT -> elem a xt

intersect xs [] = []
intersect [] ys = []
intersect xs@(x : xt) ys@(y : yt) =
	case compare x y of
		LT -> intersect xt ys
		EQ -> x : intersect xt yt
		GT -> intersect xs yt

union xs [] = xs
union [] ys = ys
union xs@(x : xt) ys@(y : yt) =
	case compare x y of
		LT -> x : union xt ys
		EQ -> x : union xt yt
		GT -> x : union xs yt

exclude xs [] = xs
exclude [] ys = []
exclude xs@(x : xt) ys@(y : yt) =
	case compare x y of
		LT -> x : exclude xt ys
		EQ -> exclude xt ys
		GT -> exclude xs yt

merge xs [] = xs
merge [] ys = ys
merge xs@(x : xt) ys@(y : yt) =
	case compare x y of
		LT -> x : merge xt ys
		EQ -> x : merge xt ys
		GT -> y : merge xs yt

nub [] = []
nub [x] = [x]
nub (x : xt@(y : yt)) = if x == y then nub xt else x : nub xt