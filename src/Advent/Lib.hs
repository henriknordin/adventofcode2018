module Advent.Lib
    ( getInput
    , Parser
    , parseWith
    , combinations
    , applyN
    ) where

import           Data.List       (tails)
import           Data.Void
import           System.IO       (readFile)
import           Text.Megaparsec (Parsec, parseErrorPretty, runParser)
import           Text.Printf     (printf)


getInput :: Int -> IO String
getInput i = readFile (printf "data/input%02d.txt" i)

type Parser = Parsec Void String

-- | Parse some file. Will print a pretty error message if it fails
parseWith :: Parser a -- ^ The input data parser
          -> Int      -- ^ The number of the day that should be parsed
          -> IO a     -- ^ The parsed result
parseWith p day = runParser p filepath <$> input >>= either report return
  where
    filepath = printf "data/input%02d.txt" day
    input = readFile filepath
    report e = fail (parseErrorPretty e)

combinations :: Integer -> [a] -> [[a]]
combinations 0 _   = [[]]
combinations n lst = do
  (x:xs) <- tails lst
  rest   <- combinations (n - 1) xs
  return $ x : rest

-- | Apply a function f n times to a value
applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)
