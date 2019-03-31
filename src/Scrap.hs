module Scrap where

import Data.Time (getCurrentTime)
import qualified Data.List as L
import System.Random
import System.IO.Unsafe
import Pokerb
import Data.HashMap.Lazy
import Data.Char
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Monad.Trans.Except

hashMap = fromList [(1 :: Int, 'a'), (2, 'b'), (3, 'c')]

awesome = do
    print hashMap
    print $ keys hashMap
    print $ elems hashMap

    --print $ null hashMap
    --print $ size hashMap

    print $ member 1 hashMap
    print $ member 5 hashMap

    --print $ lookup 1 hashMap
    --print $ lookup 5 hashMap

    print $ hashMap ! 1
    print $ lookupDefault 'N' 5 hashMap

testMe :: Maybe String
       -> Maybe String
       -> Either String String
testMe fir sec = do
  z <- case fir of
         Nothing -> Left "Fir was Nothing!!"
         Just x  -> Right $ "Fir was Amazing!!" ++ x
  case sec of
    Nothing -> Left "Sec was Nothing!!"
    Just x  -> Right $ z ++ " - " ++ "Sec was Amazing!!" ++ x

type GameValue = Int
type GameState = (Bool, Int)
 
playGame :: String -> State GameState GameValue
playGame []     = do
    (_, score) <- get
    return score
 
playGame (x:xs) = do
    (on, score) <- get
    case x of
         'a' | on -> put (on, score + 1)
         'b' | on -> put (on, score - 1)
         'c'      -> put (not on, score)
         _        -> put (on, score)
    playGame xs
 
startState = (False, 0) 

fooo = print $ runState (playGame "abcaaacbbcabbab") startState

cap :: String -> String
cap = L.map toUpper

rev :: String -> String
rev = L.reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = fmap rev cap

tupled :: String -> (String, String)
tupled = (,) <$> rev <*> cap

tupled2 :: String -> (String, String)
tupled2 = do
  x <- rev
  y <- cap
  return (x, y)

tupled3 :: String -> (String, String)
tupled3 = rev >>= fmap (,) cap

boop = (*2)
doop = (+10)
bip :: Integer -> Integer
bip = boop . doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop
