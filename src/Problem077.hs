--------------------------------------------------------------------------------
--  Problem 77
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  71
--------------------------------------------------------------------------------

module Problem077 where

import Base ( (|>), y, dp )
import qualified Juke ( primes'' )

solutionFrom [] = solutionFrom ["5000"]
solutionFrom [nS] = solutionGen (read nS)

solutionGen n = [1 ..] |> filter good |> head
	where
	good k = summariesCx k >= n



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
	Juke.primes'' |> takeWhile (<= m) |> reverse
