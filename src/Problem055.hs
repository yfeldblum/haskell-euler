--------------------------------------------------------------------------------
--  Problem 55
--------------------------------------------------------------------------------
--  A number that never forms a palindrome through the reverse and add process
--  (reversing the digits of the number and adding to the number) is called a
--  Lychrel number. Note that a palindrome can be a Lychrel number. For this
--  problem, we may assume a number is Lychrel if it does not form a palindrome
--  through the reverse and add process within 50 iterations of the process. How
--  many Lychrel numbers are there below ten-thousand?
--------------------------------------------------------------------------------
--  249
--------------------------------------------------------------------------------

module Problem055 where

import Juke ( itol, ltoi )

solutionFrom [] = solution

solution = fromIntegral $ length $ filter (isLychrel 10) $ [1..9999]

step base n = n + r
	where
	ln = itol base n
	lr = reverse ln
	r = ltoi base lr

isPalindrome base a = la == reverse la
	where
	la = itol base a

isLychrel base n = go 50 n where
	go 0 n = True
	go a n =
		let s = step base n in
		if isPalindrome base s then False else
		go (a-1) s
