module Advent05
  ( advent05
  ) where

import           Data.Char  (toUpper)

import           Advent.Lib (getInput)

fullReduce :: String -> String
fullReduce xs = let xs' = reduce xs
                in if length xs == length xs'
                      then xs
                      else fullReduce xs'

reduce :: String -> String
reduce [] = []
reduce [x] = [x]
reduce (x:y:xs) = if x /= y && toUpper x == toUpper y
                     then reduce xs
                     else x:reduce (y:xs)

removeAll :: String -> Char -> String
removeAll xs c = filter (\x -> not (x == toUpper c || x == c)) xs

parseInput :: String -> String
parseInput = head . lines

answer1 :: String -> Int
answer1 = length

answer2 :: String -> Int
answer2 xs = minimum $ map (length . fullReduce . removeAll xs) ['a' .. 'z']

advent05 :: IO ()
advent05 = do
  input <- fullReduce . parseInput <$> getInput 05
  putStrLn $ "Advent 05-1: " ++ show (answer1 input) -- 10888
  putStrLn $ "Advent 05-2: " ++ show (answer2 input) -- 6952
