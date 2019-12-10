{-# Language ViewPatterns #-}

module Day10 (part1, part2) where

{-  Advent of Code 2019 - Day 10 - https://adventofcode.com/2019/day/10

-}

import Control.Arrow ((>>>), (&&&))
import Data.List     (delete, groupBy, maximumBy, sortOn, (\\))
import Data.Ord      (comparing)
import qualified Data.Set as S


{- Types -}

type Pos       = (Int, Int)   -- col, row
type Input     = (Int, [Pos]) -- side length and positions of hashes

data Output    = Part1 Pos Int
               | Part2 Int

instance Show Output where
  show (Part1 pos count) = show (pos, count)
  show (Part2 num)       = show num


{- Parsing -}

parse :: String -> Input
parse (lines -> grid) = (side, list)
  where
    list   = map fst hashes
    hashes = filter (snd >>> (=='#')) zipped
    zipped = zip coords (concat grid)
    coords = concat [[ (col, row) | col <- [0..side-1]]
                                  | row <- [0..side-1]]
    side   = length grid


{- Part 1 -}

calc1 :: Input -> Output
calc1 (side, hashes) = Part1 winner count
  where
    (winner, count) = maximumBy (comparing snd) all
    all             = [ (pos, try pos) | pos <- hashes ]

    -- find the number of other asteroids visible from this particular one
    try :: Pos -> Int
    try pos@(col,row) = go $ sortOn (manhattan pos)
                                    (delete pos hashes)
      where
        -- the list of asteroids passed to go has been sorted by manhattan distance, so
        -- the next in the list is the closest to us and can't possibly be occluded.
        -- count it as a visible asteroid and continue with the rest of the list
        -- minus anything occluded by it
        go :: [Pos] -> Int
        go []     = 0
        go (p:ps) = 1 + go (p `occluding` ps)

        occluding :: Pos -> [Pos] -> [Pos]
        occluding (col',row') others = others \\ hits
          where
            -- vector from the candidate asteroid to the occluding one. shrink it to the
            -- "unit vector", ie, the smallest non-zero magnitude that lands on the grid
            vector = normalize (col,row) (col',row')

            -- the other places on the grid to check are multiples of vect
            hits = takeWhile bounded $ drop 1 $ iterate step (col',row')

            step    (c,r) = (c + fst vector, r + snd vector) 
            bounded (c,r) = and [ c >= 0  , r >= 0   ,
                                  c < side, r < side ]
          
            -- takeWhile :: (a -> Bool) -> [a] -> [a]
            -- iterate   :: (a -> a) -> a -> [a]



{- Part 2 -}

calc2 :: Input -> Output
calc2 (side, hashes) = Part2 result
  where
    result = 0


{- Common -}

manhattan :: Pos -> Pos -> Int
manhattan (c1,r1) (c2,r2) = abs (c1-c2) + abs (r1-r2)

normalize :: Pos -> Pos -> Pos
normalize (col,row) (col',row') = vector
  where
    divisor  = gcd dc dr
    vector   = (dc `div` divisor,
                dr `div` divisor)            
    (dc, dr) = (col'-col,
                row'-row)



{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

