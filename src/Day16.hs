module Day16 (part1, part2) where

{-  Advent of Code 2019 - Day 16 - https://adventofcode.com/2019/day/16 -}

import Control.Arrow   ((>>>))
import Data.Char       (digitToInt)


{- Types -}

type Input     = [Int]
data Output    = Output [Int]

instance Show Output where
  show (Output value) = show value


{- Parsing -}

parse :: String -> Input
parse = lines >>> head >>> map digitToInt


{- Methods -}

base :: [Int]
base = [0,1,0,-1]

pattern :: Int -> [Int]
pattern i = drop 1 $ concatMap (replicate i) (cycle base)

fft :: [Int] -> [Int]
fft nums = map (calc >>> lastdigit) [1..length nums]
  where
    calc :: Int -> Int
    calc i = sum $ zipWith (*) nums (pattern i)

    lastdigit :: Int -> Int
    lastdigit i = (abs i) `mod` 10


calc1 :: Input -> Output
calc1 digits = Output result
  where
    target = 100
    result = take 8 $ head $ drop target $ iterate fft digits


calc2 :: Input -> Output
calc2 steps = Output result
  where
    result = [-2]


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

