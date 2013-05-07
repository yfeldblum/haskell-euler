--------------------------------------------------------------------------------
--  Problem 54
--------------------------------------------------------------------------------
--  The associated file contains one thousand random hands dealt to two players.
--  By the rules of poker hand rankings, how many hands does Player 1 win?
--------------------------------------------------------------------------------
--  376
--------------------------------------------------------------------------------

module Problem054 where

import Data.Maybe ( fromJust )
import Data.List ( findIndex, groupBy )

import Base ( (|>), on, unwrap )
import Sort ( quicksort )

solutionFrom [] = solutionFrom ["data/Problem054.poker.txt"]
solutionFrom [filenameS] = solution filenameS

data Card = Card { face :: Integer , suit :: Integer } deriving (Eq, Show)
instance Ord Card where
	compare (Card f1 s1) (Card f2 s2) =
		case compare f1 f2 of
			LT -> LT
			GT -> GT
			EQ -> case compare s1 s2 of
				LT -> LT
				GT -> GT
				EQ -> EQ

data Hand = Hand [Card] deriving (Eq, Show)
data Round = Round [Hand]
data Game = Game [Round]

readPoker pokerRaw = map (\ (a, b) -> [Hand $ quicksort a, Hand $ quicksort b]) $ map (splitAt 5) $ map (map readCard) $ map words $ lines $ pokerRaw

solution filename = fromIntegral $ length $ filter (== 0) $ map winner $ readPoker $ unwrap $ readFile filename

winner round = fst $ foldl1 maxHand $ zip [0..] round
	where
	maxHand a@(i1, v1) b@(i2, v2) = if v1 <= v2 then b else a

faces = [0..12]
suits = [0..3]

readFace f = fromIntegral $ fromJust $ findIndex (== f) "23456789TJQKA"
readSuit s = fromIntegral $ fromJust $ findIndex (== s) "DCHS"
readCard [f,s] = Card (readFace f) (readSuit s)

instance Ord Hand where
	compare a b = head $ dropWhile (== EQ) $ map (compareHandCheckRank a b) $ ranks

compareHandCheckRank :: Hand -> Hand -> Rank -> Ordering
compareHandCheckRank a@(Hand ha) b@(Hand hb) rank =
	case (rank a, rank b) of
		(Nothing, Nothing) -> EQ
		(Nothing, Just rb) -> LT
		(Just ra, Nothing) -> GT
		(Just ra, Just rb) ->
			let highCardsInRank = dropWhile (\ (a, b) -> face a == face b) $ zip (reverse ra) (reverse rb) in
			case highCardsInRank of
				(a, b):_ -> compare a b
				[] ->
					let highCards = dropWhile (\ (a, b) -> face a == face b) $ zip (reverse ha) (reverse hb) in
					case highCards of
						(a, b):_ -> compare a b
						[] -> EQ

type Rank = Hand -> Maybe [Card]

ranks :: [Rank]
ranks = reverse $
	[ checkHighCard
	, checkOnePair
	, checkTwoPairs
	, checkTriple
	, checkStraight
	, checkFlush
	, checkFullHouse
	, checkFourOfAKind
	, checkStraightFlush
	, checkRoyalFlush
	]

checkHighCard :: Rank
checkHighCard (Hand cards) = if null cards then Nothing else Just ([last cards])

checkOnePair :: Rank
checkOnePair (Hand cards) = if null pairs then Nothing else Just (last pairs)
	where
	pairs = books 2 cards

checkTwoPairs :: Rank
checkTwoPairs (Hand cards) = if length pairs < 2 then Nothing else Just (concat $ reverse $ take 2 $ reverse $ pairs)
	where
	pairs = books 2 cards

checkTriple :: Rank
checkTriple (Hand cards) = if null triples then Nothing else Just (last triples)
	where
	triples = books 3 cards

checkStraight :: Rank
checkStraight (Hand cards) = if not $ isRun cards then Nothing else Just cards

checkFlush :: Rank
checkFlush (Hand cards) = if not $ isFlush cards then Nothing else Just cards

checkFullHouse :: Rank
checkFullHouse (Hand cards) = if null pairs || null triples then Nothing else Just (last pairs ++ last triples)
	where
	pairs = books 2 cards
	triples = books 3 cards

checkFourOfAKind :: Rank
checkFourOfAKind (Hand cards) = if null quads then Nothing else Just (last quads)
	where
	quads = books 4 cards

checkStraightFlush :: Rank
checkStraightFlush (Hand cards) = if not $ isFlush cards && isRun cards then Nothing else Just cards

checkRoyalFlush :: Rank
checkRoyalFlush (Hand cards) = if not $ isFlush cards && isRun cards && face (last cards) == last faces then Nothing else Just cards

books size cards =
	cards
	|>	groupBy ((==) `on` face)
	|>	filter ((== size) . length)

isRun [] = True
isRun (x:[]) = True
isRun (x:y:xs) = face y - face x == 1 && isRun (y:xs)

isFlush [] = True
isFlush (x:xs) = let s = suit x in all (== s) $ map suit xs
