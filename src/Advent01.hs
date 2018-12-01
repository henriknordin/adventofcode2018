--- Day 1: Chronal Calibration ---
module Advent01
  ( advent01
  ) where

import           Data.List  (scanl')
import           Data.Maybe (fromJust)
import qualified Data.Set   as Set (Set, empty, insert, member)

import           Advent.Lib (getInput)

parseInput :: String -> [Int]
parseInput = map parseInt . lines
  where
    parseInt :: String -> Int
    parseInt ('+':xs) = read xs
    parseInt xs       = read xs

answer1 :: [Int] -> Int
answer1 = sum

answer2 :: [Int] -> Int
answer2 = fromJust . firstRepeating . scanl' (+) 0 . cycle

firstRepeating :: [Int] -> Maybe Int
firstRepeating = go Set.empty
  where
    go :: Set.Set Int -> [Int] -> Maybe Int
    go _ [] = Nothing
    go seen (x:xs)
      | x `Set.member` seen = Just x
      | otherwise           = go (Set.insert x seen) xs

advent01 :: IO ()
advent01 = do
  input <- parseInput <$> getInput 01
  putStrLn $ "Advent 01-1: " ++ show (answer1 input) -- 400
  putStrLn $ "Advent 01-2: " ++ show (answer2 input) -- 232
