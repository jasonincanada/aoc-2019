module Day01 (part1, part2) where

{-  Advent of Code 2019 - Day 1 - https://adventofcode.com/2019/day/1

    First day, using roughly the same format as my Kattis code solutions
-}

import Control.Arrow ((>>>))
import Scanner


{- Types -}

type Input  = [Int]
data Output = Output Int

instance Show Output where
  show (Output fuel) = show fuel


{- Parsing -}

parseInput :: Scanner Input
parseInput = many int


{- Methods -}

calc1 :: Input -> Output
calc1 masses = Output result
  where
    result = sum (f <$> masses)
    f mass = mass `div` 3 - 2


calc2 :: Input -> Output
calc2 masses = Output result
  where
    result = sum (f <$> masses)

    f mass | this >= 0  = this + f this
           | otherwise  = 0
      where
        this = mass `div` 3 - 2


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
  where
    parse = runLineScanner parseInput


part2 :: String -> String
part2 = parse >>> calc2 >>> show
  where
    parse = runLineScanner parseInput

