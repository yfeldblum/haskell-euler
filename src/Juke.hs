module Juke (
	fibonaccis, primes, primes', primes'', isPrime, isPrime',
	factors, factors', divides, primeFactors, multiFactors, multiProduct,
	coprime, totient, factorial, fibonacci,
	permutations, sublists, submultilists, partitions, partitionsx, partitionsx',
	fractionExpansion,
	ltoi, itol,
	isqrt
) where

import Data.Array (array, (!))
import Data.Ix (range)
import Data.List (delete, foldl, foldl', foldl1, foldl1', group, nub, elemIndex)
import Data.Maybe (fromJust)

import qualified Rising
import Sort
import Base

--  fibs
fibonaccis :: [Integer]
fibonaccis = 0 : 1 : zipWith (+) fibonaccis (tail fibonaccis)

--  primes

isPrime primes n = n >= head primes && (all notDivisor $ takeWhile small $ primes) where
	small p = p * p <= n
	notDivisor p = not $ p `divides` n

isPrime' primes n = Rising.elem n primes

primes :: (Integral a) => [a]
primes = 2 : filter (isPrime primes) [3, 5 ..]

primes' :: (Integral a) => [a]
primes' = [2, 3, 5] ++ Rising.exclude [7, 9 ..] nonprimes where
	nonprimes :: (Integral a) => [a]
	nonprimes = foldr1 f $ map g $ tail $ primes' where 
		f (x : xt) ys = x : Rising.merge xt ys
		g p = [p * p' | p' <- [p, p + 2 ..] ]

data People a = VIP a (People a) | Crowd [a]

primes'' :: (Integral a) => [a]
primes'' = 2 : 3 : Rising.exclude [5, 7 ..] nonprimes where
	nonprimes :: (Integral a) => [a]
	nonprimes = serve $ foldTree mergeP $ map multiples $ tail $ primes'' where
		multiples p = vip [ p * k | k <- [p, p + 2 ..] ]
		vip (x : xs) = VIP x $ Crowd xs
		serve (VIP x xs) = x : serve xs
		serve (Crowd xs) = xs
	foldTree :: (a -> a -> a) -> [a] -> a
	foldTree f ~(x : xs) = f x $ foldTree f $ pairs $ xs where
		pairs ~(x : ~(y : ys)) = f x y : pairs ys
	mergeP :: (Ord a) => People a -> People a -> People a
	mergeP (VIP x xt) ys = VIP x $ mergeP xt ys
	mergeP (Crowd xs) (Crowd ys) = Crowd $ Rising.merge xs ys
	mergeP xs@(Crowd ~(x : xt)) ys@(VIP y yt) =
		case compare x y of
			LT -> VIP x $ mergeP (Crowd xt) ys
			EQ -> VIP x $ mergeP (Crowd xt) yt
			GT -> VIP y $ mergeP xs yt

--  factors
factors :: (Integral a) => a -> [a]
factors n = [ x | x <- [1 .. n], 0 == rem n x ]

factors' :: (Integral a) => a -> [a]
factors' n | n < 1 = []
factors' n = quicksort $ map multiProduct $ submultilists $ multiFactors $ n

k `divides` n = n `rem` k == 0

--  primeFactors
primeFactors :: (Integral a) => a -> [a]
primeFactors n = go n primes where
	go n ps@(p : pt) =
		if q < 1 then [] else
		if r == 0 then p : go q ps else
		go n pt
		where (q, r) = quotRem n p

--  multiFactors
multiFactors :: (Integral a) => a -> [(a, Int)]
multiFactors n = [ (head xs, length xs) | xs <- group $ primeFactors $ n ]

multiProduct :: (Integral a) => [(a, Int)] -> a
multiProduct xs = product $ map (uncurry (^)) $ xs

multiCombine f xs ys = filter k $ go f $ combine xs ys where
	k (n, m) = m /= 0
	go f xs = map k xs where k (n, (m1, m2)) = (n, f m1 m2)
	combine [] [] = []
	combine [] ys = map k ys where k (n, m) = (n, (0, m))
	combine xs [] = map k xs where k (n, m) = (n, (m, 0))
	combine xs@((xn, xm) : xt) ys@((yn, ym) : yt) =
		case compare xn yn of
			LT -> (xn, (xm, 0)) : combine xt ys
			GT -> (yn, (0, ym)) : combine xs yt
			EQ -> (xn, (xm, ym)) : combine xt yt

wideLcm = multiProduct . foldl1 (multiCombine max) . map multiFactors
wideGcd = multiProduct . foldl1 (multiCombine min) . map multiFactors

--  Coprimes are numbers such that their gcd is 1.
--  Coprimes have no factors in common, other than 1.
coprime :: (Integral a) => Predicate a
coprime a b = 1 == gcd a b

--  Euler's "phi function" or "totient function"
--  The number of units in Z_n.
totient :: (Integral a) => a -> Int
totient n = length [ x | x <- [0 .. n - 1], coprime n x ]

factorial :: (Integral a) => a -> a
factorial n = go 1 n where
	go k 0 = k
	go k n = go (k * n) (n - 1)

fibonacci :: (Integral a) => a -> a
fibonacci n = go 0 1 n where
    go a _ 0 = a
    go a b n = go b (a + b) (n - 1)

--  examples of y-combinator and dp-combinator
--  fib: exponential version
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)
--  fib: version suitable for the y-combinator and dp-combinator:
--  replace recursion with incomplete recursion
fib' rec 0 = 0
fib' rec 1 = 1
fib' rec n = rec (n-2) + rec (n-1)
--  y-combinator version
fiby n = y fib' n
--  dp-combinator version
fiby' n = dp (0, n) fib' n
--  the better algorithm
fibg n = go 0 1 n where
	go a b 0 = a
	go a b n = go b (a + b) (n - 1)
--  the better algorithm, list-based
fiba n = fibonaccis !! n
--  Dijkstra's recursion
--  * F(2n) = (2 F(n-1) + F(n)) * F(n)
--  * F(2n-1) = F(n-1)^2 + F(n)^2
fibd :: (Integral a) => a -> a
fibd 0 = 0
fibd 1 = 1
fibd n =
	let (q, r) = quotRem n 2 in
	let [j, k, l] = map fibd $ map (+ q) $ [-1, 0, 1] in
	case r of
		0 -> (2 * j + k) * k
		1 -> k ^ 2 + l ^ 2
--  Exponentiation - Prelude already defines an efficient exponentiation for Num
--  * |1 1|^n   |F(n+1) F(n) |
--  * |1 0|   = | F(n) F(n-1)|
data M2 a = M2 a a a a deriving (Eq, Show)
instance (Num a, Ord a) => Num (M2 a) where
	(M2 a b c d) + (M2 a' b' c' d') = (M2 (a + a') (b + b') (c + c') (d + d'))
	(M2 a b c d) * (M2 a' b' c' d') = (M2 (a * a' + c * b') (b * a' + d * b') (a * c' + c * d') (b * c' + d * d'))
	negate (M2 a b c d) = M2 (-a) (-b) (-c) (-d)
	abs m = if signum m == -1 then negate m else m
	signum (M2 a b c d) = case compare (a * d) (b * c) of { LT -> -1 ; EQ ->  0 ; GT ->  1 }
	fromInteger n = M2 n' 0 0 n' where n' = fromInteger n
fibx :: (Integral a) => a -> a
fibx n = r where M2 _ r _ _ = (M2 1 1 1 0) ^ n

--	get a list of all the permutations of a given list
--	the size of the list of permutations, given a list of length n, is n!
--	the permutations are given in the lexicographic order determined by the given list

permutations :: (Eq a) => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x : ys | x <- xs, ys <- permutations $ delete x $ xs ]

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (h : t) = ys ++ [ h : y | y <- ys ] where ys = sublists t

submultilists :: (Integral n) => [(a, n)] -> [[(a, n)]]
submultilists [] = [[]]
submultilists ((h, hi) : t) = [ (h, hi') : ys | hi' <- [0, k hi .. hi], ys <- submultilists t ] where
	k x | x < 0     = -1
	k x | otherwise = 1

multCombinations xs ys = filter k $ go xs ys where
	k (n, i) = i /= 0
	go xs [] = xs
	go [] ys = ys
	go xs@(x@(xn, xi) : xt) ys@(y@(yn, yi) : yt) =
		case compare xn yn of
			LT -> x : go xt ys
			EQ -> (xn, xi + yi) : go xt yt
			GT -> y : go xs yt

multCombinations' xs ys = filter k $ reverse $ go [] xs ys where
	k (n, i) = i /= 0
	go a [] [] = a
	go a (x : xt) [] = go (x : a) xt []
	go a [] (y : yt) = go (y : a) [] yt
	go a xs@(x@(xn, xi) : xt) ys@(y@(yn, yi) : yt) =
		case compare xn yn of
			LT -> go (x : a) xt ys
			GT -> go (y : a) xs yt
			EQ -> go ((xn, xi + yi) : a) xt yt

quotCombinations xs ys = multCombinations xs (map k ys) where
	k (xn, xi) = (xn, -xi)

quotCombinations' xs ys = multCombinations' xs (map k ys) where
	k (xn, xi) = (xn, -xi)

prodCombinations xs = foldl multCombinations [] xs

prodCombinations' xs = foldl' multCombinations' [] xs

partitions :: (Integral a) => [a] -> a
partitions xs = n `quot` d where
	n = factorial $ sum $ xs
	d = product $ map factorial $ xs

partitionsx xs = multiProduct $ quotCombinations n d where
	n = factorial $ sum $ xs
	d = prodCombinations $ map factorial $ xs
	factorial n = prodCombinations $ map multiFactors $ [1 .. n]

partitionsx' xs = multiProduct $ quotCombinations' n d where
	n = factorial $ sum $ xs
	d = prodCombinations $ map factorial $ xs
	factorial n = prodCombinations' $ map multiFactors $ [1 .. n]

--  Fraction Expansion
--  taken/adapted from http://xorlogic.blogspot.com/2007_09_01_archive.html
--  (q, f, r) = fractionExpansion b n d
--  q is the whole number part of n / d
--  f is the non-repeating part of the decimal expansion (or the expansion in base b) of n / d
--  r is the repeating part of the decimal expansion (or the expansion in base b) of n / d
fractionExpansion :: (Integral a) => a -> a -> a -> (a, [a], [a])
fractionExpansion base num den = (q, fpart, rpart) where
	(q, r) = num `quotRem` den
	(fpart, rpart) = go num [] [r]
	go num fpart seen =
		let (q, r) = num `quotRem` den in
		if r == 0 then (reverse fpart, []) else
		let n = num `rem` den * base in
		let m = n `quot` den in
		let fpart' = m : fpart in
		let num' = n - den * m in
		case elemIndex num' seen of
			Just k  -> splitAt k (reverse fpart')
			Nothing -> go num' fpart' (seen ++ [num'])

ltoi b list = go 0 0 b (reverse list) where
--	go (accum :: Integer) (place :: Integer) (base :: Integer) (revList :: Integer
	go a _ _ [] = a
	go a p b (x:xt) = go (a + b ^ p * x) (p + 1) b xt

itol b n = go [] b n where
	go a b 0 = a
	go a b n = go (r : a) b q where
		(q, r) = quotRem n b

--  Integers
isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral
