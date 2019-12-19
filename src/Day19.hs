{-# Language LambdaCase, ViewPatterns #-}

module Day19 (part1, part2) where

{-  Advent of Code 2019 - Day 19 - https://adventofcode.com/2019/day/19 -}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import qualified Data.Map.Strict as M
import           Data.List.Split     (splitOn)

import           Intcode


{- Types -}

type BeamMap = M.Map Pos Bool
type Pos     = (Int, Int)  -- x/y

data Drone   = Drone { beamMap   :: BeamMap
                     , intcode   :: Intcode
                     }

type Input   = RAM
data Output  = Part1 Int
             | Part2 Int

instance Show Output where
  show (Part1 count) = show count
  show (Part2 value) = show value



{- Parsing -}

parse :: String -> Input
parse = split >>> map (read >>> Value) >>> toMap
  where
    split = splitOn ","
    toMap = M.fromList . zip (Addr <$> [0..])


{- Common -}

initDrone :: Intcode -> Drone
initDrone comp = Drone M.empty comp


-- ask the drone if a coordinate is within the tractor beam
check :: Pos -> State Drone Bool
check (col,row) = do
  comp  <- gets intcode

  -- create the program (in the Intcode state, not Drone)
  let program =    addInput col
                >> addInput row
                >> step
  
  case runState program comp of
    (SignalOut sig, _) -> return (sig == 1)


{- Part 1 -}

calc1 :: Input -> Output
calc1 opcodes = Part1 result
  where
    comp    = initIntcode opcodes [] True
    drone   = initDrone comp
    plan    = beamMap $ execState search drone
    result  = M.size $ M.filter (==True) plan


search :: State Drone ()
search = do

  let coords = concat [[ (col,row) | col <- [0..49]]
                                   | row <- [0..49]]

  -- try each coord, recording our results
  forM_ coords $ \pos -> do
    beam <- check pos
    modify $ \c -> c { beamMap = M.insert pos beam (beamMap c) }



{- Part 2 -}

calc2 :: Input -> Output
calc2 opcodes = Part2 result
  where
    comp      = initIntcode opcodes [] True
    drone     = initDrone comp
    (col,row) = evalState locate drone
    result    = col * 10000 + row


-- find the upper-left coordinate of the first 100x100 block that fits the ship
locate :: State Drone Pos
locate = try 200 0

  where
    try :: Int -> Int -> State Drone Pos
    try row col = do

      -- move to the right until we have the left-most part of the beam for this row
      beam <- check (col,row)

      if not beam
      then try row (col+1)

      -- check if the upper right part of the 100x100 ship is also within the beam
      else do let c = col + (100-1)
              let r = row - (100-1)

              beam <- check (c,r)

              if not beam
              then try (row+1) col
              else return (col,r)



{- Rendering -}

render :: BeamMap -> String
render beamMap = unlines [[ point (col,row) | col <- [0..49]]
                                            | row <- [0..49]]
  where
    point :: Pos -> Char
    point pos = case M.lookup pos beamMap of
                  Nothing     -> ' '
                  Just False  -> '.'
                  Just True   -> '#'


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show


{-  jason@ubuntu18d:~/aoc2019$ time stack exec aoc2019-exe
    234
    9290812

    real    0m1.452s
    user    0m1.438s


    #.................................................
    ..................................................
    ..................................................
    ..................................................
    .....#............................................
    ......#...........................................
    .......#..........................................
    ........#.........................................
    .........##.......................................
    ..........##......................................
    ...........##.....................................
    ............##....................................
    .............###..................................
    ..............###.................................
    ...............###................................
    ................###...............................
    .................####.............................
    ..................####............................
    ...................####...........................
    ....................#####.........................
    .....................#####........................
    ......................#####.......................
    .......................#####......................
    ........................######....................
    .........................######...................
    ..........................######..................
    ...........................######.................
    ............................#######...............
    .............................#######..............
    ..............................#######.............
    ...............................#######............
    ................................########..........
    .................................########.........
    ..................................########........
    ...................................#########......
    ....................................#########.....
    .....................................#########....
    ......................................#########...
    .......................................##########.
    ........................................##########
    .........................................#########
    ..........................................########
    ...........................................#######
    ............................................######
    .............................................#####
    ..............................................####
    ...............................................###
    ................................................##
    .................................................#
    ..................................................
-}

