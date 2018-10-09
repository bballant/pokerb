module Main where

import Data.Time (getCurrentTime)
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

testHandRank :: [Card] -> PokerHandRank -> String
testHandRank hand expectedRank =
  (show expectedRank) ++ ": " ++ (show $ handRank5 hand == expectedRank)

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
