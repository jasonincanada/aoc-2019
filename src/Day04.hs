module Day04 (part1, part2) where

{-  Advent of Code 2019 - Day 4 - https://adventofcode.com/2019/day/4

-}

import Control.Arrow   ((>>>))
import Data.List.Split (splitOn)
import Scanner


{- Types -}

type Input     = (Int, Int)
data Output    = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

parse :: String -> Input
parse = splitOn "-" >>> map read >>> toTuple
  where
    toTuple (a:b:_) = (a, b)


{- Methods -}

valid :: String -> Int -> Bool
valid num part = if part == 1
                 then and [ twoAdjacent num
                          , increasing  num ]
                 else and [ twoAdjacent2 ("-" ++ num ++ "-")
                          , increasing  num ]
  where
    twoAdjacent [x] = False
    twoAdjacent (x:y:rest)
      | x == y      = True
      | otherwise   = twoAdjacent (y:rest)

    twoAdjacent2 (a:x:y:b:rest)
      | x == y
          && a /= x
          && b /= y = True
      | otherwise   = twoAdjacent2 (x:y:b:rest)
    twoAdjacent2 _ = False

    increasing [x] = True
    increasing (x:y:rest)
      | x > y      = False
      | otherwise  = increasing (y:rest)


calc1 :: Input -> Output
calc1 (from, to) = Output count
  where
    count = length [ num | num <- [from..to], valid (show num) 1 ]


calc2 :: Input -> Output
calc2 (from, to) = Output count
  where
    count = length [ num | num <- [from..to], valid (show num) 2 ]


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

