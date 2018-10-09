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
cardsForRank r = map (\s -> Card r s) [Diamonds ..]

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
    isAStraight     = take (length cards) [(head ranks) ..] == ranks
    isAHighStraight = (head ranks == Ten) && isAStraight
