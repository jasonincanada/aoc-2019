{-# Language ViewPatterns #-}

module Day10 (part1, part2, cast) where

{-  Advent of Code 2019 - Day 10 - https://adventofcode.com/2019/day/10 -}

import Control.Arrow   ((>>>), (&&&))
import Data.Function   ((&))
import Data.List       (delete, maximumBy, sortOn, (\\))
import Data.List.Extra (groupOn)
import Data.Ord        (comparing)


{- Types -}

type Pos       = (Int, Int)        -- col, row
type Input     = (Int, Int, [Pos]) -- grid width, height, positions of asteroids

data Output    = Part1 Pos Int     -- position of best asteroid, count of visible others
               | Part2 Int         -- 200th asteroid hit by the laser

instance Show Output where
  show (Part1 pos count) = show (pos, count)
  show (Part2 number)    = show number


{- Parsing -}

parse :: String -> Input
parse (lines -> grid) = (width, height, list)
  where
    list   = map fst hashes
    hashes = filter (snd >>> (=='#')) zipped
    zipped = zip coords (concat grid)
    coords = concat [[ (col, row) | col <- [0..width-1]]
                                  | row <- [0..height-1]]
    width  = length $ head grid
    height = length grid


{- Part 1 -}

calc1 :: Input -> Output
calc1 (width, height, hashes) = Part1 winner count
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
            bounded (c,r) = and [ c >= 0   , r >= 0     ,
                                  c < width, r < height ]
          
            -- takeWhile :: (a -> Bool) -> [a] -> [a]
            -- iterate   :: (a -> a) -> a -> [a]



{- Part 2 -}

type Angle = Double

calc2 :: Input -> Output
calc2 (width, height, asteroids) = Part2 result
  where
    result  = fst target * 100 + snd target

    -- hand-copy the result from part 1, the coordinate of our base asteroid
    base    = (11, 13)

    target  = sortOn (manhattan base)     -- order asteroid field by closest first
                >>> drop 1                -- don't consider the base asteroid
                >>> map (angleTo &&& id)  -- pair them up with their angles from base
                >>> sortOn fst            -- sort by angles
                >>> groupOn fst           -- group by angle!
                >>> map (map snd)         -- forget the angle
                >>> lase                  -- rotate the laser until all asteroids hit
                >>> drop (200-1)          -- get the 200th
                >>> head
                $ asteroids


    angleTo :: Pos -> Angle
    angleTo (c, r)
      | c == col && r < row = negate pi
      | otherwise           = uncurry atan2 $ prep $ cast $ normalize base (c,r)
      where
        -- prepare for atan2 by swapping x,y and converting to double
        prep (x,y) = (fromIntegral y, fromIntegral x)

        (col, row) = base


    -- skim the tops of each group of same-angled asteroids until there's none left
    lase :: [[Pos]] -> [Pos]
    lase []   = []
    lase list = map head list ++ lase rest
      where
        rest = map tail >>> filter (not.null) $ list


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

-- cast from the problem's coordinate system to regular Cartesian
cast :: Pos -> Pos
cast (c,r) = (r,-c)


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

