module Main where

import System.Environment

main = do
	(num:args) <- getArgs
	putStrLn $ show $ solution num args
