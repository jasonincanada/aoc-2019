module Day08 (part1, part2) where

{-  Advent of Code 2019 - Day 8 - https://adventofcode.com/2019/day/8 -}

import Control.Arrow   ((>>>), (&&&))
import Data.Char       (digitToInt)
import Data.List       (group, groupBy, sort, sortBy)
import Data.List.Split (chunksOf)
import Data.Ord        (comparing)


{- Types -}

type Layer  = Int
type Row    = Int
type Col    = Int
type Digit  = Int

type Input  = [Digit]

data Output = Count Int       -- part 1
            | Picture [Digit] -- part 2

instance Show Output where
  show (Count count)    = show count
  show (Picture digits) = unlines $ chunksOf width (map display digits)
    where
      display 0 = '.'
      display 1 = '#'


{- Parsing -}

parse :: String -> Input
parse = init >>> map digitToInt


-- globals given by the problem page
width  = 25
height = 6


{- Methods -}

type Summary = [((Layer, Digit), Int)]

calc1 :: Input -> Output
calc1 digits = Count result
  where
    result = count layer 1 (summary list)
           * count layer 2 (summary list)

    layer  = fewest 0 (summary list)

    list   = [ (layer, digit) | (i, digit) <- zip [0..]
                                                  digits,

                                let layer = i `div` (width*height) ]

    -- count each of the layer/digit combos
    summary :: [(Layer, Digit)] -> Summary 
    summary = id >>> sort                  -- Ord a => [ a ] -> [ a      ]
                 >>> group                 -- Eq  a => [ a ] -> [[a]     ]
                 >>> map (head &&& length) --          [[c]] -> [(c, Int)]

    -- find the layer containing the fewest number of a given digit
    fewest :: Digit -> Summary -> Layer
    fewest d = id >>> filter (fst >>> snd >>> (==d))
                  >>> sortBy (comparing snd)
                  >>> map (fst >>> fst)
                  >>> head

    count :: Layer -> Digit -> Summary -> Int
    count layer digit = id >>> filter (fst >>> fst >>> (==layer))
                           >>> filter (fst >>> snd >>> (==digit))
                           >>> map snd
                           >>> head

    -- example summary: [ ( (0,0), 16  ),
    --                    ( (0,1), 10  ),
    --                    ( (0,2), 124 ),
    --                    ( (1,0), 10  ),
    --                    ( (1,1), 9   ),
    --                    ( (1,2), 4   ),
    --                    ( (2,0), 3   ),
    --                    ...
    --                    ( (27,2), 33 )
    --                  ]


calc2 :: Input -> Output
calc2 digits = Picture $ flatten list
  where
    list :: [ ((Row, Col), Digit) ]
    list  = [ ((row, col), digit) | (i, digit) <- zip [0..]
                                                      digits,

                                    let (row, col) = (i `div` width `mod` height,
                                                      i `mod` width)
                                  ]

    flatten :: [ ((Row,Col), Digit) ] -> [Digit]
    flatten = id >>> sortBy (comparing fst)
                 >>> groupBy (\a b -> fst a == fst b) -- Eq a => [(a, b)] -> [[(a, b)]]
                 >>> map (dropWhile (snd >>> (==2)))  -- remove transparent cells
                 >>> map (head >>> snd)


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show


{-  jason@ubuntu18d:~/aoc2019$ time stack exec aoc2019-exe
    1806
    ..##..##..####.###...##..
    ...#.#..#.#....#..#.#..#.
    ...#.#..#.###..#..#.#..#.
    ...#.####.#....###..####.
    #..#.#..#.#....#.#..#..#.
    .##..#..#.#....#..#.#..#.


    real    0m0.363s
    user    0m0.328s
    sys     0m0.025s
-}

