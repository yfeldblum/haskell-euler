--------------------------------------------------------------------------------
--  Problem 19
--------------------------------------------------------------------------------
--  How many Sundays fell on the first of the month during the twentieth century
--  (1 Jan 1901 to 31 Dec 2000)? Note that: 1 Jan 1900 was a Monday; the months
--  April, June, September, November have 30 days, February has 28 days or, on a
--  leap year, 29 days, and all the other months have 31 days; and a leap year
--  occurs on any year evenly divisible by 4, but not on a century, unless it is
--  divisible by 400.
--------------------------------------------------------------------------------
--  171
--------------------------------------------------------------------------------

module Problem019 where

import Data.List (foldr1)
import System.IO.Unsafe (unsafePerformIO)

import Base

solutionFrom [] = return $ show $ solution

solution =
	Date Mon 1 Jan 1900
	|>	iterate nextDate
	|>	dropWhile (\ (Date w d m y) -> y < 1901)
	|>	takeWhile (\ (Date w d m y) -> y < 2001)
	|>	filter (\ (Date w d m y) -> w == Sun && d == 1)
	|>	length

data Weekday = Sun | Mon | Tues | Wed | Thur | Fri | Sat deriving (Eq, Show)
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq, Show)
data Date = Date Weekday Integer Month Integer deriving (Eq, Show)

weekdays = cycle [Sun, Mon, Tues, Wed, Thur, Fri, Sat]
months = cycle [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]

nextWeekday w = go w weekdays where
	go w (w':ws) =
		if w == w'
		then head ws
		else go w ws
nextMonth m = go m months where
	go m (m':ms) =
		if m == m'
		then head ms
		else go m ms

nextDate (Date w d m y) = Date w' d' m' y' where
	w' = nextWeekday w
	d' = if d <= dm then d + 1 else 1
	m' = if d <= dm then m else nextMonth m
	y' = if d <= dm || m /= Dec then y else y + 1
	dm = days m y

days Jan _ = 31
days Feb y | y `rem` 400 == 0 = 29
days Feb y | y `rem` 100 == 0 = 28
days Feb y | y `rem` 4   == 0 = 29
days Feb y | otherwise        = 28
days Mar _ = 31
days Apr _ = 30
days May _ = 31
days Jun _ = 30
days Jul _ = 31
days Aug _ = 31
days Sep _ = 30
days Oct _ = 31
days Nov _ = 30
days Dec _ = 31

