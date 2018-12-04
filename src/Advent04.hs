{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Advent04
  ( advent04
  ) where

import           Data.Char                  (isDigit)
import           Data.List
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust, isJust)
import           Data.Ord                   (comparing)

import           Text.Megaparsec            (eof, someTill)
import           Text.Megaparsec.Char       (eol)
import qualified Text.Megaparsec.Char.Lexer as L (charLiteral, decimal)

import           Advent.Lib                 (Parser, parseWith)

-- [1518-11-05 00:03] Guard #99 begins shift
-- [1518-11-05 00:45] falls asleep
-- [1518-11-05 00:55] wakes up

data Timestamp = Timestamp
  { tYear  :: Int
  , tMonth :: Int
  , tDay   :: Int
  , tHour  :: Int
  , tMin   :: Int
  } deriving (Show, Eq, Ord)

data Obs = Obs
  { obsTimestamp :: Timestamp
  , obsNote      :: String
  } deriving (Show, Eq, Ord)

data GuardObs = GuardObs
  { gId    :: String
  , gSleep :: [Int]
  } deriving Show

obsesParser :: Parser [Obs]
obsesParser = do
  obses <- someTill obsParser eof
  pure obses
  where
    obsParser :: Parser Obs
    obsParser = do
      --obsTimestamp <- "[" *> someTill L.charLiteral "]"
      obsTimestamp <- timestampParser
      obsNote <- " " *> someTill L.charLiteral eol
      pure Obs {..}
    timestampParser :: Parser Timestamp
    timestampParser = do
      tYear <- "[" *> L.decimal
      tMonth <- "-" *> L.decimal
      tDay <- "-" *> L.decimal
      tHour <- " " *> L.decimal
      tMin <- ":" *> L.decimal <* "]"
      pure Timestamp {..}

parseGuardObs :: [Obs] -> [GuardObs]
parseGuardObs [] = []
parseGuardObs (x:xs) = let (ys, zs) = span ((not . isSubsequenceOf "Guard") . obsNote) xs
                       in GuardObs (obsNote x) (sleep ys) : parseGuardObs zs

sleep :: [Obs] -> [Int]
sleep []       = []
sleep [_]      = []
sleep (a:b:xs) = [(tMin $ obsTimestamp a)..(tMin (obsTimestamp b) - 1)] ++ sleep xs

answer1 :: [GuardObs] -> Int
answer1 xs = let sleepPattern = concatMap (\(GuardObs _ l) -> l) $ filter (\g -> gId g == mostSleepyGuard) xs
                 maxMinute = getMostFrequent sleepPattern
              in getNumericId mostSleepyGuard * fst (fromJust maxMinute)
  where
    totalSleepByGuards = foldl' (\b a -> Map.insertWith (+) (gId a) (sum $ gSleep a) b) Map.empty xs
    mostSleepyGuard = fst $ maximumBy (comparing snd) $ Map.assocs totalSleepByGuards


getNumericId :: String -> Int
getNumericId = read . filter isDigit

answer2 :: [GuardObs] -> Int
answer2 xs = let sleepByGuards = foldl' (\b a -> Map.insertWith (++) (gId a) (gSleep a) b) Map.empty xs
                 sleepByGuardsMost = map (\(k, v) -> let qs = getMostFrequent v in (k, qs)) $ Map.assocs sleepByGuards
                 sleepGuard = maximumBy (comparing (snd . snd)) $ map (\(k, v) -> (k, fromJust v)) $ filter (isJust . snd) sleepByGuardsMost
              in getNumericId (fst sleepGuard) * fst (snd sleepGuard)

getMostFrequent :: [Int] -> Maybe (Int, Int)
getMostFrequent [] = Nothing
getMostFrequent xs = Just (head ys, length ys)
  where
    ys = (maximumBy (comparing length) . group . sort) xs

advent04 :: IO ()
advent04 = do
  obses <- parseWith obsesParser 04
  let guardObses = parseGuardObs (sortOn obsTimestamp obses)

  putStrLn $ "Advent 04-1: " ++ show (answer1 guardObses) -- 98680
  putStrLn $ "Advent 04-2: " ++ show (answer2 guardObses) -- 9763
