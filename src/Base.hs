{-# OPTIONS
	-XMultiParamTypeClasses
	-XBangPatterns
#-}

module Base (
	(|>), Wrapped (unwrap), ListLike (toList, fromList),
	Predicate, Comparison,
	on,
	y, dp,
	minimum', maximum', sum', product',
	splitOn,
	unmerge
) where

import Data.Ix (range)
import Data.Array ((!), array)
import Data.List (foldl', foldl1, foldl1')
import Data.Maybe (fromJust)
import Data.Tuple (uncurry)
import System.IO.Unsafe (unsafePerformIO)



class Wrapped k where
	unwrap :: k a -> a

instance Wrapped IO where
	unwrap = unsafePerformIO

instance Wrapped Maybe where
	unwrap = fromJust

instance Wrapped [] where
	unwrap = head

instance Wrapped (Either a) where
	unwrap = \ (Right x) -> x



class ListLike k a where
	toList :: k a -> [a]
	fromList :: [a] -> k a



infixl 0 |>
(|>) = flip ($)



op `on` f = \ a b -> f a `op` f b



--  y-combinator: fixed-point incomplete-recursion combinator
y f = f $ y f

--  dp-combinator: memoizing y-combinator
dp bounds f = (memo !) where
	memo = tabulate bounds (f (memo !))
	tabulate bounds f = array bounds [ (i, f i) | i <- range bounds ]



-- get some type signatures in there

type Predicate a = a -> a -> Bool
type Comparison a = a -> a -> Ordering



--	Strict folds

minimum' xs = foldl1' min xs
maximum' xs = foldl1' max xs

sum' = foldl' (+) 0
product' = foldl' (*) 1



--	Splits

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim [] = [[]]
splitOn delim (c:cs) | c == delim = [] : splitOn delim cs
splitOn delim (c:cs) | otherwise  = (c : h) : t where (h : t) = splitOn delim cs

unmerge xs = go xs xs [] where
	go [] ys zs = (reverse zs, ys)
	go [x] ys zs = (reverse zs, ys)
	go (x:xx:xs) (y:ys) zs = go xs ys (y:zs)



average' [] = error ""
average' xs = fromIntegral n / fromIntegral d where
	(n, d) = _average' xs
	_average' xs = go 0 0 xs where
		go !n !d [] = (n, d)
		go !n !d (x:xs) = go (n + x) (d + 1) xs



