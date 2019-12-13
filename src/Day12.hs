module Day12 (part1, part2) where

{-  Advent of Code 2019 - Day 12 - https://adventofcode.com/2019/day/12

    Work in progress, part 2 is not yet complete
-}

import Control.Arrow   ((>>>))
import Control.Monad   (replicateM_)
import Control.Monad.State


{- Types -}

type Position  = [Int]
type Velocity  = [Int]
type Moon      = (Position, Velocity)
type Input     = [Moon]

data Output    = Output Int

instance Show Output where
  show (Output energy) = show energy


{- Parsing -}

-- <x=0, y=4, z=0>
-- <x=-10, y=-6, z=-14>
parse :: String -> [Moon]
parse = lines >>> map numbers >>> map addVelocity
  where
    numbers :: String -> [Int]
    numbers = words >>> map (filter isDigit >>> read)
      where
        isDigit :: Char -> Bool
        isDigit = flip elem "-0123456789"

    addVelocity :: Position -> Moon
    addVelocity pos = (pos, [0,0,0])



{- Methods -}

calc1 :: Input -> Output
calc1 moons = Output result
  where
    -- not sure why this concat needs to be here
    -- [[1100,9108,1620,1672]]
    result         = sum $ concat [ map energy stated ]
    (stated, hist) = execState (replicateM_ 1000 gravity) (moons, [moons])


gravity :: State ([Moon], [[Moon]]) ()
gravity = do
  (moons, hist) <- get

  let velocities' = map (velocity' moons) moons
  let moons' = zipWith z moons velocities'

  put (moons', hist ++ [moons])


z :: Moon -> [Int] -> Moon
z ([x,y,z], _) [dx,dy,dz] = ([x+dx, y+dy, z+dz], [dx,dy,dz])

velocity' :: [Moon] -> Moon -> [Int]
velocity' moons ([x,y,z], [dx,dy,dz]) = [dx+a, dy+b, dz+c]
  where
    a = gravity' x (map (\([x,_,_], _) -> x) moons)
    b = gravity' y (map (\([_,x,_], _) -> x) moons)
    c = gravity' z (map (\([_,_,x], _) -> x) moons)

gravity' :: Int -> [Int] -> Int
gravity' p ps = sum [ case p `compare` p' of
                        LT ->  1
                        GT -> -1
                        _  ->  0
                                  | p' <- ps ]

energy :: Moon -> Int
energy ([x,y,z], [dx,dy,dz]) = pot * kin
  where
    pot = abs x  + abs y  + abs z
    kin = abs dx + abs dy + abs dz


calc2 :: Input -> Output
calc2 steps = Output result
  where
    result = -2


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

