--------------------------------------------------------------------------------
--  Problem 59
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  107359
--------------------------------------------------------------------------------

module Problem059 where

import Data.List ( isInfixOf )
import Data.Word ( Word8 )
import Data.Bits ( xor )
import qualified Data.Char as Char ( chr, ord )

import Base ( splitOn )

solutionFrom [] = solutionFrom ["data/Problem059.ciphertext.txt"]
solutionFrom [filenameS] = do
	text <- readFile filenameS
	return $ show $ solution text

readCiphertext f = map (fromIntegral :: Integer -> Word8) $ map read $ splitOn ',' f

solution text = sum $ map toInteger $ head $
	[ plaintext
	| key <- keys
	, let plaintext = decrypt key ciphertext
	, let plaintextString = map (Char.chr . fromIntegral) plaintext
	, " and " `isInfixOf` plaintextString
	]
	where
	ciphertext = readCiphertext $ text

keyAlphabet = map (fromInteger . fromIntegral . Char.ord) ['a'..'z']
keys = [ [a,b,c] | a <- keyAlphabet, b <- keyAlphabet, c <- keyAlphabet ]

decrypt key ciphertext = zipWith xor (cycle key) ciphertext
