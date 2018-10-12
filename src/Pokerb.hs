module Pokerb where

import Data.List
import System.Random
import System.IO.Unsafe

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Eq, Ord, Show, Enum, Bounded)

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq, Ord, Show, Enum, Bounded)

data Card =
  Card Rank Suit
  deriving (Eq, Ord)

instance Show Card where
  show (Card r s) = show r ++ " of " ++ show s

data PokerHandRank = HighCard
                   | APair
                   | TwoPair
                   | ThreeOfAKind
                   | Straight
                   | Flush
                   | FullHouse
                   | FourOfAKind
                   | StraightFlush
                   | RoyalFlush
                   deriving (Eq, Ord, Show, Enum, Bounded)

cardsForRank :: Rank -> [Card]
cardsForRank r = map (\s -> Card r s) [Clubs ..]

-- 52 cards, fresh out of the wrapper
deckO'Cards :: [Card]
deckO'Cards = [Two ..] >>= cardsForRank

shuffleList :: [a] -> IO [a]
shuffleList [] = return [] -- An empty list cannot be shuffled more
shuffleList [x] = return [x] -- A list with one element cannot be shuffled more
shuffleList as = do
    i <- randomRIO (0,length as-1)
    shuffledRest <- shuffleList (take i as ++ drop (i+1) as)
    return $ (as !! i) : shuffledRest

shuffledDeckOfCards = shuffleList deckO'Cards
unsafeShuffledCardsOnce = unsafePerformIO (shuffleList deckO'Cards)

mapcat f xs = xs >>= f

headNTail :: [a] -> Int -> ([a], a, [a])
headNTail xs n = let (y:ys) = drop n xs
                 in (take n xs, y, ys)

deal :: [a] -> Int -> Int -> ([[a]], [a])
deal shuffledCards numCards numPeople =
  let dealF :: [[a]] -> [a] -> Int -> Int -> ([[a]], [a])
      dealF dealt deck      0  0  = (dealt, deck)
      dealF dealt []        _  _  = dealF dealt [] 0 0
      dealF dealt deck      cn 0  = dealF dealt deck (cn-1) numPeople
      dealF dealt (c1:deck) cn pn =
        let (h, x, t) = headNTail dealt (numPeople - pn)
        in dealF (h ++ [(c1:x)] ++ t) deck cn (pn - 1)
  in dealF (take numPeople $ repeat []) shuffledCards numCards numPeople

sameSuit :: Card -> Card -> Bool
sameSuit (Card x _) (Card x' _) = x == x'

groupAndSortOfAKind :: [Card] -> [[Card]]
groupAndSortOfAKind cards =  reverse . (sortBy lenSorter) . (groupBy sameSuit) . sort $ cards
  where lenSorter :: [a] -> [a] -> Ordering
        lenSorter l l' = compare (length l) (length l')

cardIdentity :: [Card] -> [Card]
cardIdentity c = c

ranksForCards :: [Card] -> [Rank]
ranksForCards cards = map (\(Card r _) -> r) cards

suitsForCards :: [Card] -> [Suit]
suitsForCards cards = map (\(Card _ s) -> s) cards

sortedRanksForCards :: [Card] -> [Rank]
sortedRanksForCards cards = sort . ranksForCards $ cards

ensureLengthFive :: [a] -> [a]
ensureLengthFive cards = if length cards /= 5
                         then error "Rotten"
                         else cards

isAtLeastNOfAKind :: [Card] -> Int -> Bool
isAtLeastNOfAKind cards n = (length $ head groupedSortedCards) >= n
  where groupedSortedCards = groupAndSortOfAKind cards

isSecondPair :: [Card] -> Bool
isSecondPair cards = (length . head . tail $ groupedSortedCards) >= 2
  where groupedSortedCards = groupAndSortOfAKind cards

isAFlush :: [Card] -> Bool
isAFlush cards = all (\s -> s == head suits) suits
  where suits = suitsForCards $ ensureLengthFive cards

isAStraight :: [Card] -> Bool
isAStraight cards = take 5 [(head ranks) ..] == take 5 ranks
  where ranks = sortedRanksForCards $ ensureLengthFive cards

isAHighStraight :: [Card] -> Bool
isAHighStraight cards = head ranks == Ten && isAStraight cards
  where ranks = sortedRanksForCards $ ensureLengthFive cards

-- assumes 5 cards in the hand
fiveCardHandRank :: [Card] -> PokerHandRank
fiveCardHandRank cards
  | length cards /= 5                               = error "Rotten"
  | isAHighStraight cards     && isAFlush cards     = RoyalFlush
  | isAStraight cards         && isAFlush cards     = StraightFlush
  | isAtLeastNOfAKind cards 4                       = FourOfAKind
  | isAtLeastNOfAKind cards 3 && isSecondPair cards = FullHouse
  | isAFlush cards = Flush
  | isAStraight cards                               = Straight
  | isAtLeastNOfAKind cards 3                       = ThreeOfAKind
  | isAtLeastNOfAKind cards 2 && isSecondPair cards = TwoPair
  | isAtLeastNOfAKind cards 2                       = APair
  | otherwise                                       = HighCard

combinationOfN :: Eq a => Int -> [a] -> [[a]]
combinationOfN n cs =
  if n == 1 then (group cs)
  else cs >>= \c -> map (c:) (combinationOfN (n-1) cs)

uniqueCombinationsOfN :: Ord a => Int -> [a] -> [[a]]
uniqueCombinationsOfN n cs =
  nub . (map sort) . filter ((n==) . length . nub) $ combs
  where combs = combinationOfN n cs

findHands cards = reverse . sort . (map fiveCardHandRank) . uniqueCombinationsOfN 5 $ cards


-------------------------- old

combinationOfNCards' :: Eq a => Int -> [a] -> [[a]]
combinationOfNCards' n cards =
  if n == 1 then (group cards)
  else combinationStep [] cards (combinationOfNCards' (n-1) cards)
  where combinationStep :: [[a]] -> [a] -> [[a]] -> [[a]]
        combinationStep acc (c:cs) prevCombs =
          if length cs == 0 then acc
          else combinationStep (acc ++ map (c:) prevCombs) cs prevCombs

uniqueCombinationsOfN' :: Ord a => Int -> [a] -> [[a]]
uniqueCombinationsOfN' n cs =
  nub . (map sort) . filter ((n==) . length . nub) $ combs
  where combs = combinationOfNCards' n cs

-- assumes 5 cards in the hand
handRank5 :: [Card] -> PokerHandRank
handRank5 cards
  | isAHighStraight && isAFlush     = RoyalFlush
  | isAStraight     && isAFlush     = StraightFlush
  | isNOfAKind 4                    = FourOfAKind
  | isNOfAKind 3    && isSecondPair = FullHouse
  | isAFlush                        = Flush
  | isAStraight                     = Straight
  | isNOfAKind 3                    = ThreeOfAKind
  | isNOfAKind 2    && isSecondPair = TwoPair
  | isNOfAKind 2                    = APair
  | otherwise                       = HighCard
  where
    nOfAKind :: [[Card]] -> Int -> Bool
    nOfAKind grpd n = (length $ head grpd) == n
    sorted          = sort cards
    grouped         = groupAndSortOfAKind sorted
    isNOfAKind      = nOfAKind grouped
    isSecondPair    = nOfAKind (tail grouped) 2
    ranks           = map (\(Card r _) -> r) sorted
    suits           = map (\(Card _ s) -> s) sorted
    isAFlush        = all (\s -> s == head suits) suits
    isAStraight     = hasAStraight cards
    isAHighStraight = (head ranks == Ten) && isAStraight

hasAStraightOld :: [Card] -> Bool
hasAStraightOld cards = any isA5CardStraight (fiveCardSeqs [] sortedCards)
  where sortedCards = sort cards
        isA5CardStraight :: [Card] -> Bool
        isA5CardStraight cs = take 5 [(head ranks) ..] == ranks
          where ranks = map (\(Card r _) -> r) cs
        fiveCardSeqs :: [[Card]] -> [Card] -> [[Card]]
        fiveCardSeqs acc cs = if length cs < 5 then acc
                              else fiveCardSeqs ((take 5 cs) : acc) (tail cs)

hasAStraight :: [Card] -> Bool
hasAStraight cards = go (sort cards)
  where go cs
         | length cs < 5                   = False
         | isAStraight (take 5 cs) = True
         | otherwise                       = go (tail cards)
