{-# Language LambdaCase, MultiWayIf #-}

module Day09 (part1, part2) where

{-  Advent of Code 2019 - Day 9 - https://adventofcode.com/2019/day/9 -}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Bool           (bool)
import qualified Data.Map as M
import           Data.List.Split     (splitOn)
import           Intcode


{- Types -}

type Input   = RAM
data Output  = Output [Int]

instance Show Output where
  show (Output value) = show value


{- Parsing -}

parse :: String -> Input
parse = split >>> map (read >>> Value) >>> toMap
  where
    split = splitOn ","
    toMap = M.fromList . zip (Addr <$> [0..])


{- Part 1 -}

calc1 :: Input -> Output
calc1 opcodes = Output result
  where
    result = outputs $ execState step start

    -- 1 is the starting input given by the problem
    start  = initIntcode opcodes [Value 1] False


{- Part 2 -}

calc2 :: Input -> Output
calc2 opcodes = Output result
  where
    result = outputs $ execState step start

    -- 2 is the starting input given by the problem
    start  = initIntcode opcodes [Value 2] False


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show


{-  jason@ubuntu18d:~/aoc2019$ time stack exec aoc2019-exe
    [2453265701]
    [80805]

    real    0m0.493s
    user    0m0.470s
    sys     0m0.004s
-}

