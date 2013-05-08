--------------------------------------------------------------------------------
--  Problem 17
--------------------------------------------------------------------------------
--  If all the numbers from 1 to 1000 inclusive were written out in words, how
--  many letters would be used?
--------------------------------------------------------------------------------
--  21124
--------------------------------------------------------------------------------

module Problem017 where

import Data.Char (isAlpha)

solutionFrom [] = return $ show $ solution

solution = fromIntegral $ sum $ map countLetters $ map toWords $ map show $ [1 .. 1000]

countLetters = length . filter isAlpha

--  Ones place
toWords "0" = ""
toWords "1" = "one"
toWords "2" = "two"
toWords "3" = "three"
toWords "4" = "four"
toWords "5" = "five"
toWords "6" = "six"
toWords "7" = "seven"
toWords "8" = "eight"
toWords "9" = "nine"
--  Tens place
toWords ['0', d1] = toWords [d1]
toWords "10" = "ten"
toWords "11" = "eleven"
toWords "12" = "twelve"
toWords "13" = "thirteen"
toWords "14" = "fourteen"
toWords "15" = "fifteen"
toWords "16" = "sixteen"
toWords "17" = "seventeen"
toWords "18" = "eighteen"
toWords "19" = "nineteen"
toWords ['2', d1] = "twenty-" ++ toWords [d1]
toWords ['3', d1] = "thirty-" ++ toWords [d1]
toWords ['4', d1] = "forty-" ++ toWords [d1]
toWords ['5', d1] = "fifty-" ++ toWords [d1]
toWords ['6', d1] = "sixty-" ++ toWords [d1]
toWords ['7', d1] = "seventy-" ++ toWords [d1]
toWords ['8', d1] = "eighty-" ++ toWords [d1]
toWords ['9', d1] = "ninety-" ++ toWords [d1]
toWords ['0', '0', '0'] = ""
toWords ['0', d2, d1] = "and " ++ toWords [d2, d1]
toWords [d3, '0', '0'] = toWords [d3] ++ " hundred"
toWords [d3, d2, d1] = toWords [d3] ++ " hundred and " ++ toWords [d2, d1]
toWords "1000" = "one thousand"
