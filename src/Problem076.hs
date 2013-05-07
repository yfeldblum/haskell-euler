--------------------------------------------------------------------------------
--  Problem 76
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  190569291
--------------------------------------------------------------------------------

module Problem076 where

import Debug.Trace

import Base ( (|>), y, dp )

solutionFrom [] = solutionFrom ["100"]
solutionFrom [nS] = solutionGen (read nS)

solutionGen n = s n - 1 where
	s = summariesCx



summaries n = y f (n, n) where
	f = summaries'
summariesx n = dp b f (n, n) where
	f = summaries'
	b = ((0, 0), (n, n))

summaries' f (_, 0) = [[]]
summaries' f (maxk, n) =
	[ k : s
	| k <- numsForSummary maxk n
	, s <- f (k, n - k)
	]



summariesC n = y f (n, n) where
	f = summariesC'
summariesCx n = dp b f (n, n) where
	f = summariesC'
	b = ((0, 0), (n, n))

summariesC' f (_, 0) = 1
summariesC' f (maxk, n) =
	[ s
	| k <- numsForSummary maxk n
	, let s = f (k, n - k)
	]
	|> sum



numsForSummary maxk n =
	let m = min maxk n in
	[1 .. m] |> reverse



{-

0 |-> 0
1 |-> 1
2 |-> 2
3 |-> 3
4 |-> 5
5 |-> 7
6 |-> 11
7 |-> 15
8 |-> 22
9 |-> 30

0 |-> 0

1 |-> 1
	1 = 1

2 |-> 2
	2 = 2
	2 = 1 + 1

3 |-> 3
	3 = 3
	3 = 2 + 1
	3 = 1 + 1 + 1

4 |-> 5
	4 = 4
	4 = 3 + 1
	4 = 2 + 2
	4 = 2 + 1 + 1
	4 = 1 + 1 + 1 + 1

5 |-> 7
	5 = 5
	5 = 4 + 1
	5 = 3 + 2
	5 = 3 + 1 + 1
	5 = 2 + 2 + 1
	5 = 2 + 1 + 1 + 1
	5 = 1 + 1 + 1 + 1 + 1

6 |-> 11
	6 = 6
	6 = 5 + 1
	6 = 4 + 2
	6 = 4 + 1 + 1
	6 = 3 + 3
	6 = 3 + 2 + 1
	6 = 3 + 1 + 1 + 1
	6 = 2 + 2 + 2
	6 = 2 + 2 + 1 + 1
	6 = 2 + 1 + 1 + 1 + 1
	6 = 1 + 1 + 1 + 1 + 1 + 1

7 |-> 15
	7 = 7
	7 = 6 + 1
	7 = 5 + 2
	7 = 5 + 1 + 1
	7 = 4 + 3
	7 = 4 + 2 + 1
	7 = 4 + 1 + 1 + 1
	7 = 3 + 3 + 1
	7 = 3 + 2 + 2
	7 = 3 + 2 + 1 + 1
	7 = 3 + 1 + 1 + 1 + 1
	7 = 2 + 2 + 2 + 1
	7 = 2 + 2 + 1 + 1 + 1
	7 = 2 + 1 + 1 + 1 + 1 + 1
	7 = 1 + 1 + 1 + 1 + 1 + 1 + 1

8 | -> 22
	8 = 8
	8 = 7 + 1
	8 = 6 + 2
	8 = 6 + 1 + 1
	8 = 5 + 3
	8 = 5 + 2 + 1
	8 = 5 + 1 + 1 + 1
	8 = 4 + 4
	8 = 4 + 3 + 1
	8 = 4 + 2 + 2
	8 = 4 + 2 + 1 + 1
	8 = 4 + 1 + 1 + 1 + 1
	8 = 3 + 3 + 2
	8 = 3 + 3 + 1 + 1
	8 = 3 + 2 + 2 + 1
	8 = 3 + 2 + 1 + 1 + 1
	8 = 3 + 1 + 1 + 1 + 1 + 1
	8 = 2 + 2 + 2 + 2
	8 = 2 + 2 + 2 + 1 + 1
	8 = 2 + 2 + 1 + 1 + 1 + 1
	8 = 2 + 1 + 1 + 1 + 1 + 1 + 1
	8 = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

9 | -> 30
	9 = 9
	9 = 8 + 1
	9 = 7 + 2
	9 = 7 + 1 + 1
	9 = 6 + 3
	9 = 6 + 2 + 1
	9 = 6 + 1 + 1
	9 = 5 + 4
	9 = 5 + 3 + 1
	9 = 5 + 2 + 2
	9 = 5 + 2 + 1 + 1
	9 = 5 + 1 + 1 + 1 + 1
	9 = 4 + 4 + 1
	9 = 4 + 3 + 2
	9 = 4 + 3 + 1 + 1
	9 = 4 + 2 + 2 + 1
	9 = 4 + 2 + 1 + 1 + 1
	9 = 4 + 1 + 1 + 1 + 1 + 1
	9 = 3 + 3 + 3
	9 = 3 + 3 + 2 + 1
	9 = 3 + 3 + 1 + 1 + 1
	9 = 3 + 2 + 2 + 2
	9 = 3 + 2 + 2 + 1 + 1
	9 = 3 + 2 + 1 + 1 + 1 + 1
	9 = 3 + 1 + 1 + 1 + 1 + 1 + 1
	9 = 2 + 2 + 2 + 2 + 1
	9 = 2 + 2 + 2 + 1 + 1 + 1
	9 = 2 + 2 + 1 + 1 + 1 + 1 + 1
	9 = 2 + 1 + 1 + 1 + 1 + 1 + 1 + 1
	9 = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-}