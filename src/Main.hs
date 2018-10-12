module Main where

import Data.Time (getCurrentTime)
import Data.List
import System.Random
import System.IO.Unsafe
import Pokerb

printTime = do
  time <- getCurrentTime
  putStrLn (show time)

main :: IO ()
main = do
  putStrLn "The time is: "
  printTime
  putStrLn "------------"
  runTests

---- Poker

aRoyalFlush    = [Card Ten Hearts, Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts]
aStraightFlush = [Card Two Hearts, Card Three Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts]
aFourOfAKind   = [Card Two Spades, Card Two Hearts, Card Two Diamonds, Card Two Clubs, Card Queen Hearts]
aFullHouse     = [Card Two Spades, Card Two Hearts, Card Two Diamonds, Card King Clubs, Card King Hearts]
aFlush         = [Card Two Spades, Card Four Spades, Card Ace Spades, Card Jack Spades, Card Nine Spades]
aStraight      = [Card Two Spades, Card Three Hearts, Card Four Diamonds, Card Five Clubs, Card Six Hearts]
aThreeOfAKind  = [Card Two Spades, Card Two Hearts, Card Two Diamonds, Card King Clubs, Card Queen Hearts]
aTwoPair       = [Card Two Spades, Card Two Hearts, Card Jack Diamonds, Card Jack Clubs, Card Queen Hearts]
aPair          = [Card Two Spades, Card Two Hearts, Card Jack Diamonds, Card King Clubs, Card Queen Hearts]
aHighCard      = [Card Two Spades, Card Five Hearts, Card Jack Diamonds, Card King Clubs, Card Queen Hearts]

aRoyalFlush7    = [Card Two Clubs, Card Four Clubs, Card Ten Hearts, Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts]
aStraightFlush7 = [Card Two Clubs, Card Four Clubs, Card Two Hearts, Card Three Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts]
aFourOfAKind7   = [Card Two Clubs, Card Four Clubs, Card Two Spades, Card Two Hearts, Card Two Diamonds, Card Two Clubs, Card Queen Hearts]
aFullHouse7     = [Card Two Clubs, Card Four Clubs, Card Two Spades, Card Two Hearts, Card Two Diamonds, Card King Clubs, Card King Hearts]
aFlush7         = [Card Two Clubs, Card Four Clubs, Card Two Spades, Card Four Spades, Card Ace Spades, Card Jack Spades, Card Nine Spades]
aStraight7      = [Card Two Clubs, Card Four Clubs, Card Two Spades, Card Three Hearts, Card Four Diamonds, Card Five Clubs, Card Six Hearts]
aThreeOfAKind7  = [Card Two Clubs, Card Four Clubs, Card Two Spades, Card Two Hearts, Card Two Diamonds, Card King Clubs, Card Queen Hearts]
aTwoPair7       = [Card Two Clubs, Card Four Clubs, Card Two Spades, Card Two Hearts, Card Jack Diamonds, Card Jack Clubs, Card Queen Hearts]
aPair7          = [Card Two Clubs, Card Four Clubs, Card Two Spades, Card Two Hearts, Card Jack Diamonds, Card King Clubs, Card Queen Hearts]
aHighCard7      = [Card Two Clubs, Card Four Clubs, Card Two Spades, Card Five Hearts, Card Jack Diamonds, Card King Clubs, Card Queen Hearts]

testHandRank :: [Card] -> PokerHandRank -> String
testHandRank hand expectedRank =
  (show expectedRank) ++ ": " ++ (show $ fiveCardHandRank hand == expectedRank)

randomHand n = take n . unsafePerformIO . shuffleList . nub $ aStraight ++ aRoyalFlush ++ aFullHouse7 ++ aPair7
findHandsFromRandom n = findHands (randomHand n)

runTests :: IO ()
runTests = do
  putStrLn "Running tests"
  putStrLn $ testHandRank aRoyalFlush    RoyalFlush
  putStrLn $ testHandRank aStraightFlush StraightFlush
  putStrLn $ testHandRank aFourOfAKind   FourOfAKind
  putStrLn $ testHandRank aFullHouse     FullHouse
  putStrLn $ testHandRank aFlush         Flush
  putStrLn $ testHandRank aStraight      Straight
  putStrLn $ testHandRank aThreeOfAKind  ThreeOfAKind
  putStrLn $ testHandRank aTwoPair       TwoPair
  putStrLn $ testHandRank aPair          APair
  putStrLn $ testHandRank aHighCard      HighCard

runTests7 :: IO ()
runTests7 = do
  putStrLn "Running tests"
  putStrLn $ testHandRank aRoyalFlush7    RoyalFlush
  putStrLn $ testHandRank aStraightFlush7 StraightFlush
  putStrLn $ testHandRank aFourOfAKind7   FourOfAKind
  putStrLn $ testHandRank aFullHouse7     FullHouse
  putStrLn $ testHandRank aFlush7         Flush
  putStrLn $ testHandRank aStraight7      Straight
  putStrLn $ testHandRank aThreeOfAKind7  ThreeOfAKind
  putStrLn $ testHandRank aTwoPair7       TwoPair
  putStrLn $ testHandRank aPair7          APair
  putStrLn $ testHandRank aHighCard7      HighCard
