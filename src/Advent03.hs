{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Advent03
  ( advent03
  ) where

import           Data.List
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)

import           Text.Megaparsec            (eof, someTill)
import           Text.Megaparsec.Char       (eol)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

import           Advent.Lib                 (Parser, parseWith)

data Square = Square
  { sId :: !Int
  , sX  :: !Int
  , sY  :: !Int
  , sW  :: !Int
  , sH  :: !Int
  } deriving Show

squaresParser :: Parser [Square]
squaresParser = do
  squares <- someTill squareParser eof
  pure squares
  where
    squareParser :: Parser Square
    squareParser = do
      sId <- "#" *> L.decimal
      sX <- " @ " *> L.decimal
      sY <- "," *> L.decimal
      sW <- ": " *> L.decimal
      sH <- "x" *> L.decimal <* eol
      pure Square {..}

-- | Sum all the overlapping claims. An overlapping claim is defined as
-- | having a negative value in the map.
answer1 :: [Square] -> Int
answer1 xs = foldl' (\b a -> if a < 0 then b + 1 else b) 0 claims
  where
    claims = addSquares xs

addSquares :: [Square] -> Map.Map (Int, Int) Int
addSquares = foldl' addClaim Map.empty

addClaim :: Map.Map (Int, Int) Int -> Square -> Map.Map (Int, Int) Int
addClaim m _s@Square{..} =
  let coords = [(x, y) | x <- [sX .. (sX + sW - 1)],
                         y <- [sY .. (sY + sH - 1)]]
  in foldl' updateMap m coords
  where
    updateMap :: Map.Map (Int, Int) Int -> (Int, Int) -> Map.Map (Int, Int) Int
    updateMap g c = case Map.lookup c g of
                      Nothing -> Map.insert c sId g
                      Just _  -> Map.insert c (-1) g

answer2 :: [Square] -> Int
answer2 xs = let claims = addSquares xs
                 sizes  = uniqueById claims
              in sId $ fromJust
               $ find (\s -> area s == Map.findWithDefault (-1) (sId s) sizes) xs
  where
    uniqueById :: Map.Map (Int, Int) Int -> Map.Map Int Int
    uniqueById = foldl' (\b a -> Map.insertWith (\_ old -> old + 1) a 1 b) Map.empty

area :: Square -> Int
area _s@Square{..} = sW * sH

advent03 :: IO ()
advent03 = do
  squares <- parseWith squaresParser 03
  putStrLn $ "Advent 03-1: " ++ show (answer1 squares) -- 124850
  putStrLn $ "Advent 03-2: " ++ show (answer2 squares) -- 1097
