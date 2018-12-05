module Main where

import qualified Data.IntMap.Strict  as IntMap (IntMap, fromList, lookup)
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Advent01            (advent01)
import           Advent02            (advent02)
import           Advent03            (advent03)
import           Advent04            (advent04)
import           Advent05            (advent05)
--import           Advent06 (advent06)
--import           Advent07 (advent07)
--import           Advent08 (advent08)
--import           Advent09 (advent09)
--import           Advent10 (advent10)
--import           Advent11 (advent11)
--import           Advent12 (advent12)
--import           Advent13 (advent13)
--import           Advent14 (advent14)
--import           Advent15 (advent15)
--import           Advent16 (advent16)
--import           Advent17 (advent17)
--import           Advent18 (advent18)
--import           Advent19 (advent19)
--import           Advent20 (advent20)
--import           Advent21 (advent21)
--import           Advent22 (advent22)
--import           Advent23 (advent23)
--import           Advent24 (advent24)
--import           Advent25 (advent25)


days :: IntMap.IntMap (IO ())
days = IntMap.fromList
  [ ( 1, advent01)
  , ( 2, advent02)
  , ( 3, advent03)
  , ( 4, advent04)
  , ( 5, advent05)
--  , ( 6, advent06)
--  , ( 7, advent07)
--  , ( 8, advent08)
--  , ( 9, advent09)
--  , (10, advent10)
--  , (11, advent11)
--  , (12, advent12)
--  , (13, advent13)
--  , (14, advent14)
--  , (15, advent15)
--  , (16, advent16)
--  , (17, advent17)
--  , (18, advent18)
--  , (19, advent19)
--  , (20, advent20)
--  , (21, advent21)
--  , (22, advent22)
--  , (23, advent23)
--  , (24, advent24)
--  , (25, advent25)
  ]

data Options = RunDay Int
             | RunAll

runWithOptions :: Options -> IO ()
runWithOptions (RunDay day) =
  fromMaybe (putStrLn $ "No solution availabe for advent " ++ show day)
            (IntMap.lookup day days)
runWithOptions RunAll       = sequence_ days

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs { prefShowHelpOnError = True }

main :: IO ()
main = runWithOptions =<< customExecParser parserPrefs opts
  where
    parser :: Parser Options
    parser = pure RunAll
         <|> RunDay <$> option auto (long "day" <>
                                     short 'd' <>
                                     help "Which day should we run" <>
                                     metavar "INT")

    opts = info (parser <**> helper)
      (  fullDesc
      <> progDesc "If no arguments available, the solutions for all days will be calculated"
      <> header "Solutions for Advent of Code 2018")
