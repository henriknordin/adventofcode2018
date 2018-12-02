module Advent02
  ( advent02
  ) where

import           Control.Applicative (liftA2)
import           Data.List           (elem, foldl', group, sort, zip)

import           Advent.Lib          (getInput)

parseInput :: String -> [String]
parseInput = lines

answer1 :: [String] -> Int
answer1 xs = uncurry (*)
           $ foldl'(\(b1, b2) (a1, a2) -> (b1 + a1, b2 + a2)) (0, 0)
           $ map contains xs

contains :: String -> (Int, Int)
contains xs = let gs = freqs xs
              in (fromEnum $ 2 `elem` gs, fromEnum $ 3 `elem` gs)
  where
    freqs :: String -> [Int]
    freqs s1 = map length $ group $ sort s1

answer2 :: [String] -> String
answer2 xs = head
           $ map (uncurry common)
           $ filter isValidPair combos
  where
    combos = liftA2 (,) xs xs

isValidPair :: (String, String) -> Bool
isValidPair (xs, ys) = (length xs - 1) == length (common xs ys)

common :: (Eq a) => [a] -> [a] -> [a]
common xs = map fst . filter (uncurry (==)) . zip xs

advent02 :: IO ()
advent02 = do
  input <- parseInput <$> getInput 02
  putStrLn $ "Advent 02-1: " ++ show (answer1 input) -- 5681
  putStrLn $ "Advent 02-2: " ++ show (answer2 input) -- uqyoeizfvmbistpkgnocjtwld
