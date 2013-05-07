module PartitionFunction (
	p',
) where

import Base ( (|>), y, dp )

p' f 0 = 1
p' f n = pentagonalsIndex |> takeWhile good |> map term |> sum
	where
	good (i, p) = p <= n
	term (i, p) = factor i * f (n - p)
		where
		factor i | even i = -1
		factor i | odd  i =  1

pentagonal n = (n * (n * 3 - 1)) `quot` 2
pentagonalsIndex = zip nums (map pentagonal nums)
	where
	pos = [1 ..]
	neg = map (0 -) pos
	nums = interleave pos neg
	interleave (a:as) (b:bs) = a : b : interleave as bs
