--------------------------------------------------------------------------------
--  Problem 79
--------------------------------------------------------------------------------
--  73162890
--------------------------------------------------------------------------------

module Problem079 where

import Data.List ( sort, nub )

import Base ( unwrap )

solutionFrom [] = solutionFrom ["data/Problem079.keylog.txt"]
solutionFrom [filenameS] = return $ show $ solution $ parseFile filenameS

parseFile filename = lines $ unwrap $ readFile filename

solution :: [String] -> Integer
solution captures =
	read $ head $ filter (matchesCaptures captures) $ guesses alphabet
	where
	alphabet = alphabetFromCaptures captures

alphabetFromCaptures captures = nub $ sort $ concat $ captures

guesses alphabet = [ g | k <- [1..], g <- guessesOfSize k alphabet ]

guessesOfSize 0 alphabet = [[]]
guessesOfSize s alphabet = [ a:g | a <- alphabet, g <- guessesOfSize (s-1) alphabet ]

matchesCaptures captures guess = all (\ c -> matchesCapture c guess) captures

matchesCapture [] _ = True
matchesCapture _ [] = False
matchesCapture (c:apture) (g:uess) | c == g = matchesCapture apture uess
matchesCapture (c:apture) (g:uess) | otherwise = matchesCapture (c:apture) uess
