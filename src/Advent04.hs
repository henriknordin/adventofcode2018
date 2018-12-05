{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Advent04
  ( advent04
  ) where

import           Data.List
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust, isJust)
import           Data.Ord                   (comparing)

import           Text.Megaparsec            (eof, many, someTill, try)
import           Text.Megaparsec.Char       (char, eol)
import qualified Text.Megaparsec.Char.Lexer as L (charLiteral, decimal)

import           Advent.Lib                 (Parser, getInput, parseStringWith)


data GuardDuty = GuardDuty
  { gId    :: Int
  , gSleep :: [(Int, Int)] -- Interval the guard sleeps
  } deriving Show

dutiesParser :: Parser [GuardDuty]
dutiesParser = do
  duties <- someTill dutyParser eof
  pure duties
  where
    dutyParser :: Parser GuardDuty
    dutyParser = do
      gId <- someTill L.charLiteral (char '#') *> L.decimal <* someTill L.charLiteral eol
      gSleep <- many (try intervalParser)
      pure GuardDuty {..}
    intervalParser :: Parser (Int, Int)
    intervalParser = do
      -- TODO Can we have an eal at the below two lines instead?
      a <- someTill L.charLiteral ":" *> L.decimal <* "] falls asleep\n"
      b <- someTill L.charLiteral ":" *> L.decimal <* "] wakes up\n"
      pure (a, b)


answer1 :: [GuardDuty] -> Int
answer1 xs = let sleepPattern = concatMap (\(GuardDuty _ l) -> createList l) $ filter (\g -> gId g == mostSleepyGuard) xs
                 maxMinute = getMostFrequent sleepPattern
              in mostSleepyGuard * fst (fromJust maxMinute)
  where
    totalSleepByGuards :: Map.Map Int Int
    totalSleepByGuards = foldl' (\b a -> Map.insertWith (+) (gId a) (timeAsleep $ gSleep a) b) Map.empty xs
    mostSleepyGuard = fst $ maximumBy (comparing snd) $ Map.assocs totalSleepByGuards

answer2 :: [GuardDuty] -> Int
answer2 xs = let sleepByGuards = foldl' (\b a -> Map.insertWith (++) (gId a) (createList $ gSleep a) b) Map.empty xs
                 sleepByGuardsMost = map (\(k, v) -> let qs = getMostFrequent v in (k, qs)) $ Map.assocs sleepByGuards
                 sleepGuard = maximumBy (comparing (snd . snd)) $ map (\(k, v) -> (k, fromJust v)) $ filter (isJust . snd) sleepByGuardsMost
              in fst sleepGuard * fst (snd sleepGuard)


timeAsleep :: [(Int, Int)] -> Int
timeAsleep []         = 0
timeAsleep ((a,b):xs) = (b - a - 1) + timeAsleep xs

createList :: [(Int, Int)] -> [Int]
createList []         = []
createList ((a,b):xs) = [a..(b-1)] ++ createList xs

getMostFrequent :: [Int] -> Maybe (Int, Int)
getMostFrequent [] = Nothing
getMostFrequent xs = Just (head ys, length ys)
  where
    ys = (maximumBy (comparing length) . group . sort) xs

parseInput :: String -> String
parseInput = unlines . sort . lines

advent04 :: IO ()
advent04 = do
  input <- parseStringWith dutiesParser (parseInput <$> getInput 04)
  putStrLn $ "Advent 04-1: " ++ show (answer1 input) -- 98680
  putStrLn $ "Advent 04-2: " ++ show (answer2 input) -- 9763
