{-# Language LambdaCase, MultiWayIf, ViewPatterns #-}

module Day15 (part1, part2) where

{-  Advent of Code 2019 - Day 15 - https://adventofcode.com/2019/day/15 -}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Bool           (bool)
import qualified Data.Map.Strict as M
import           Data.List.Split     (splitOn)

import           Intcode hiding (signal)


{- Types -}

data Tile   = Wall
            | Floor
            | Oxygen
            deriving (Enum, Eq, Show)

type Pos    = (Int, Int)  -- x/y
type Dir    = Pos

data DroidStepResult = HitWall
                     | OkayMoved
                     | FoundOxygen
                     deriving (Enum)

data Droid  = Droid { maze      :: M.Map Pos Tile
                    , intcode   :: Intcode         -- the embedded Intcode computer
                    , position  :: Pos             -- curret location of droid
                    , direction :: Dir             -- direction the droid is pointing
                    }

type Input   = RAM
data Output  = Part1 Int
             | Part2 Int

instance Show Output where
  show (Part1 distance) = show distance
  show (Part2 rounds)   = show rounds



{- Parsing -}

parse :: String -> Input
parse = split >>> map (read >>> Value) >>> toMap
  where
    split = splitOn ","
    toMap = M.fromList . zip (Addr <$> [0..])



{- Part 1 -}

calc1 :: Input -> Output
calc1 opcodes = Part1 result
  where
    -- set up our gadgets
    comp    = initIntcode opcodes [] True
    droid   = initDroid comp

    -- search the map with the droid and return the floor map
    floor   = maze $ execState explore droid

    -- find the shortest paths from the origin to all other cells
    (ls, _) = dijkstra (0,0) floor

    -- find the distance to the Oxygen cell
    oxygen  = head $ M.keys $ M.filter (==Oxygen) floor
    result  = ls M.! oxygen


-- initialize the robot droid with an Intcode computer
initDroid :: Intcode -> Droid
initDroid comp = Droid M.empty comp (0,0) (0,-1)


{- Droid state machine -}

-- Assumptions:
--   Hallways are only one unit wide (so we don't have to search around in open areas)
--   The walls form one connected graph, so we don't walk around in a loop forever
--   after starting along an isolated wall segment

explore :: State Droid ()
explore = face north >> trace


-- trace along the left-hand wall as we move and turn, checking to see if we're back
-- to our starting point
trace :: State Droid ()
trace = do
  trystep >>= \case HitWall -> turn right >> trace
                    _       -> turn left  >> check


-- attempt to step forward one unit in the direction we're facing
trystep :: State Droid DroidStepResult
trystep = do
  this <- gets position
  dir  <- gets direction
  cpu  <- gets intcode

  -- send a movement signal to the embedded computer and save the new state
  let (SignalOut sig, cpu') = runState (stepInput $ signal dir) cpu
  modify $ \c -> c { intcode = cpu' }

  -- process the result we got back
  let result = toEnum sig
  let next   = move this dir

  case result of
    HitWall     -> found next Wall
    OkayMoved   -> found next Floor  >> at next
    FoundOxygen -> found next Oxygen >> at next

  -- we've handled the result of the step attempt in order to track our new location
  -- and what we've discovered about the map, now return it to the calling code (trace)
  -- so the higher-level logic can do whatever it needs with it
  return result


-- check if our new square is our starting one, meaning we've gone full circle.
-- if not, continue on exploring
check :: State Droid ()
check = gets position >>= \case (0,0) -> return ()
                                _     -> trace


-- turn left or right
turn :: (Dir -> Dir) -> State Droid ()
turn f = (f <$> gets direction) >>= face

-- face the robot in a given direction
face :: Dir -> State Droid ()
face dir = modify $ \c -> c { direction = dir }

-- record the position we're now at
at :: Pos -> State Droid ()
at pos = modify $ \c -> c { position = pos }

-- record a thing we found at a location
found :: Pos -> Tile -> State Droid ()
found pos tile = modify $ \c -> c { maze = M.insert pos tile $ maze c }

-- get the Intcode number that tells the robot to step in a given direction
signal :: Dir -> Int
signal ( 0,-1) = 1    -- north
signal ( 0, 1) = 2    -- south
signal (-1, 0) = 3    -- west
signal ( 1, 0) = 4    -- east

-- the direction pointing to our left depending on which direction we're facing
left :: Dir -> Dir
left ( 0,-1) = west
left ( 0, 1) = east
left (-1, 0) = south
left ( 1, 0) = north

right :: Dir -> Dir
right ( 0,-1) = east
right ( 0, 1) = west
right (-1, 0) = north
right ( 1, 0) = south

north :: Dir
north = ( 0,-1)
south = ( 0, 1)
west  = (-1, 0)
east  = ( 1, 0)

move :: Pos -> Dir -> Pos
move (x,y) (dx,dy) = (x+dx, y+dy)


{- Djikstra's shortest path algo -}

dijkstra :: Pos -> M.Map Pos Tile -> (M.Map Pos Int, Int)
dijkstra from floormap =

  let edges    = makeEdges (M.keys $ M.filter f floormap)
      f tile   = tile == Floor || tile == Oxygen
      explored = M.singleton from 0

   in go edges explored 0

  where
    go :: [(Pos, Pos)] -> M.Map Pos Int -> Int -> (M.Map Pos Int, Int)
    go edges explored count =

      let frontier = [ (from,to) | (from,to) <- edges,

                                   from `M.member`    explored,
                                   to   `M.notMember` explored ]
      in
        if | null frontier -> (explored, count)
           | otherwise     ->

              let new = M.union
                          explored
                          $ M.fromList [ (to, dist+1) | (from,to) <- frontier,
                                                        let dist = explored M.! from ]
              in  go edges new (count+1)


    -- get the set of bidirectional edges connecting the tiles in this floor
    makeEdges :: [Pos] -> [(Pos, Pos)]
    makeEdges floor =

      [ (from, to) | from <- floor,
                     to   <- floor,
                     to `elem` neighbours from ]
      where
        neighbours :: Pos -> [Pos]
        neighbours (x,y) = [ (x+dx, y+dy) | (dx,dy) <- [ north, south, west, east] ]


{- Part 2 -}

-- similar to part 1, but instead of using dijkstra to measure the shortest path from the
-- origin to the Oxygen cell, we're using it to count the number of rounds it takes to
-- fill the whole map as we expand outwards from the Oxygen cell
calc2 :: Input -> Output
calc2 opcodes = Part2 result
  where
    comp     = initIntcode opcodes [] True
    droid    = initDroid comp
    floormap = maze $ execState explore droid
    oxygen   = head $ M.keys $ M.filter (==Oxygen) floormap
    (_, c)   = dijkstra oxygen floormap
    result   = c


render :: M.Map Pos Tile -> String
render screen = unlines [[ point (col,row) | col <- [-25..25]]
                                           | row <- [-20..18]]
  where
    point pos = case M.lookup pos screen of
                  Nothing     -> ' '
                  Just Wall   -> '#'
                  Just Floor  -> '.'
                  Just Oxygen -> 'O'


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show


{-  jason@ubuntu18d:~/aoc2019$ time stack exec aoc2019-exe
    240
    322

    real    0m0.715s
    user    0m0.683s


  The path from the origin to the Oxygen cell (O), hugging the left wall:

                  .........
                  .#######.
              .....#     #...
              .####       ##.
          .....#  ...    #...
          .####   .#.    #.
        ...#  ... .#.....#.....
        .##   .#. .#####. ####.
        .#O ...#. .#...#.#.....
        .#. .###. .###.#.#.
      ...#.....#.......#.#.
      .###.#############.#.
      .#...#...........#.#.....
      .###.#.         .#. ####.
      ...#.#.   ...   ...#.....
        .#.#.   .#.      #.
      ...#.#.....#.      #... ...
      .#####.#####.      #.#. .#.
      .....#.#..... .....#.#...#.
          .#.#.     .###. ##.###.
        ...#.#.   ...#.#.. #.#.#...
        .#####.   .###.#####.#.###.
        ....... ...#...#.....#.#...
                .## ##.#.#####.#.
                ...#...#.#.....#.
                  .#.###.#####.#.
                  .#...........#.
                  .#########.###.
                  .....#.....#...
                      .#######.
                      .........


  The path from the origin to the Oxygen cell, hugging the right wall:

      ########### ######### ######### #######
     #...........#.........#.........#.......#
     #.#####.#####. ##### .###.#.#####.###.#.#
     #.#.....#.....#.....#...#.#.........#.#.#
     #.#.#####. # ####.#.#  .#.#########.#.##
     #.#.#.....#.#.....#.#...#.....#...#.#...#
     #.###. # ##.#. .#### .#########.#.#####.#
     #.#...#.#...#. .....#.....#.....#.......#
     #.#.  #.#. .#.     .#    .#.#####.#####.#
     #.#. O#... .#.     .#.....#.....#.#...#.#
     #.#. .#.   .#.     .#.#####.###.###.#.#.#
     #... ...   ...     .#.#.......#.....#.#.#
     #.                 .#.#######.#######.#.#
     #.     ........... .#.....#.#.....#...#.#
     #.     .#.#######. .#    .#.###.###.###.#
     #...   .#.#...#.#...#.....#.....#...#.#.#
      ##.   .###. .#.###.#.#######.###.###.#.#
     #...   ..... .#.....#...#...#.#...#...#.#
     #.           .######   .#. .#.#.#####.#.#
     #.....   .....#.....#  ... .#.#.#.....#.#
      ####.   .#.###.   .##     .###.#.#####.#
     #.#...   .#.#...   ..      ...#.#.#.....#
     #.#.     .###.               .#.#.#.####
     #.#.......#...             ...#.#.......#
     #.#######.#.               .###.#######.#
     #.#.....#.#...             .#.....#...#.#
     #.#.#.###.###.             .#####.#.###.#
     #.#.#.....#.#.             .#.....#.....#
     #.#.#######.#.             .#.#######.##
     #...#.....#.#.....       ...#.......#...#
     #.#####.#.#.#####.       .###.#####.###.#
     #.#.....#.#.#...#.........#...#...#.#...#
     #.###.###.#.#.#.#############.###.#.#.##
     #...#.#.#.#...#.....#.......#.#...#.#.#.#
      ##.#.#.#.#.#####.###.#.###.#.#.#.#.#.#.#
     #.#.#.#...#.....#.....#.#...#...#.#.#.#.#
     #.#.#.#.## ####.#######.###.#####.#.#.#.#
     #...#.#...#.......#...#...#.#...#.#.#...#
     #.###.###.#########.#.###.#.#.#.###.###.#
     #.....#.............#.....#...#.....#...#
      ##### ############# ##### ### ##### ###


  Running past the Oxygen cell until we return back to the origin yields
  the composite of the two:

      ########### ######### ######### #######
     #...........#.........#.........#.......#
     #.#####.#####.#######.###.#.#####.###.#.#
     #.#.....#.....#.....#...#.#.........#.#.#
     #.#.#####.#######.#.###.#.#########.#.##
     #.#.#.....#.#.....#.#...#.....#...#.#...#
     #.###.#####.#.#.#####.#########.#.#####.#
     #.#...#.#...#.#.....#.....#.....#.......#
     #.#.###.#.#.#.#####.#####.#.#####.#####.#
     #.#.#O#...#.#.#...#.#.....#.....#.#...#.#
     #.#.#.#.###.#.###.#.#.#####.###.###.#.#.#
     #...#.....#.......#.#.#.......#.....#.#.#
     #.###.#############.#.#######.#######.#.#
     #.#...#...........#.#.....#.#.....#...#.#
     #.###.#.#.#######.#.#####.#.###.###.###.#
     #...#.#.#.#...#.#...#.....#.....#...#.#.#
      ##.#.#.###.#.#.###.#.#######.###.###.#.#
     #...#.#.....#.#.....#...#...#.#...#...#.#
     #.#####.#####.#######.#.#.#.#.#.#####.#.#
     #.....#.#.....#.....#.#...#.#.#.#.....#.#
      ####.#.#.#.###.###.###.###.###.#.#####.#
     #.#...#.#.#.#...#.#...#.#.#...#.#.#.....#
     #.#.#####.###.###.#####.#.###.#.#.#.####
     #.#.......#...#...#.....#.#...#.#.......#
     #.#######.#.## ##.#.#####.#.###.#######.#
     #.#.....#.#...#...#.#.....#.#.....#...#.#
     #.#.#.###.###.#.###.#####.#.#####.#.###.#
     #.#.#.....#.#.#...........#.#.....#.....#
     #.#.#######.#.#########.###.#.#######.##
     #...#.....#.#.....#.....#...#.......#...#
     #.#####.#.#.#####.#######.###.#####.###.#
     #.#.....#.#.#...#.........#...#...#.#...#
     #.###.###.#.#.#.#############.###.#.#.##
     #...#.#.#.#...#.....#.......#.#...#.#.#.#
      ##.#.#.#.#.#####.###.#.###.#.#.#.#.#.#.#
     #.#.#.#...#.....#.....#.#...#...#.#.#.#.#
     #.#.#.#.## ####.#######.###.#####.#.#.#.#
     #...#.#...#.......#...#...#.#...#.#.#...#
     #.###.###.#########.#.###.#.#.#.###.###.#
     #.....#.............#.....#...#.....#...#
      ##### ############# ##### ### ##### ###
-}

