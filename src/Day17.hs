{-# Language LambdaCase, ViewPatterns #-}

module Day17 (part1, part2) where

{-  Advent of Code 2019 - Day 17 - https://adventofcode.com/2019/day/17 -}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Char           (chr, ord)
import qualified Data.Map.Strict as M
import           Data.List           (group, intercalate)
import           Data.List.Split     (splitOn)

import           Intcode


{- Types -}

data Tile   = Empty
            | Scaffold
            | RobotN
            | RobotS
            | RobotW
            | RobotE
            deriving (Eq, Show)

type Floor      = M.Map Pos Tile
type Pos        = (Int, Int)  -- x/y
type Dir        = Pos

type Direction  = Char   -- 'L' or 'R'
type Distance   = Int
type Segment    = (Direction, Distance)

data Movement   = Step
                | TurnLeft
                | TurnRight
                deriving (Eq, Show)

data Robot = Robot { floorplan :: M.Map Pos Tile
                   , position  :: Pos            -- current location of robot
                   , direction :: Dir            -- direction the robot is facing
                   , moves     :: [Movement]     -- individual movements
                   , segments  :: [Segment]      -- movements combined to straight paths
                   }

type Input   = RAM
data Output  = Part1 Int
             | Part2 Int

instance Show Output where
  show (Part1 alignment) = show alignment
  show (Part2 dust)      = show dust



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
    comp    = initIntcode opcodes [] False
    floor   = survey $ reverse $ outputs $ execState step comp
    result  = sum $ params floor


-- turn the list of outputs into the floor map
survey :: [Int] -> Floor
survey = M.fromList . go 0 0
  where
    go :: Int -> Int -> [Int] -> [(Pos, Tile)]
    go _   _   []  = []
    go col row ((chr -> c) : cs)
      | c == '\n'  =                 go 0 (row+1)   cs
      | c == '#'   = cell Scaffold : go (col+1) row cs
      | c == '.'   = cell Empty    : go (col+1) row cs
      | c == '^'   = cell RobotN   : go (col+1) row cs
      | c == 'v'   = cell RobotS   : go (col+1) row cs
      | c == '<'   = cell RobotW   : go (col+1) row cs
      | c == '>'   = cell RobotE   : go (col+1) row cs
      where
        cell tile = ((col,row), tile)


-- sum the scaffold intersection alignment params
params :: Floor -> [Int]
params floor = map align points
  where
    scaffolding      = M.keys $ M.filter (`elem` floortiles) floor
    floortiles       = [ Scaffold, RobotN, RobotS, RobotW, RobotE ]
    points           = filter f scaffolding
    f pos            = and [ n `elem` scaffolding | n <- neighbours pos ]
    neighbours (c,r) = map (move (c,r)) [ north, south, west, east ]
    align (c,r)      = c * r


move :: Pos -> Dir -> Pos
move (x,y) (dx,dy) = (x+dx, y+dy)

north :: Dir
north = ( 0,-1)
south = ( 0, 1)
west  = (-1, 0)
east  = ( 1, 0)



{- Part 2 -}

calc2 :: Input -> Output
calc2 opcodes = Part2 result
  where
    -- the first run of the robot is to gather all the automatic outputs (our floor plan)
    comp     = initIntcode opcodes [] False
    plan     = survey $ reverse $ outputs $ execState step comp

    -- find our robot's starting position and direction on the map
    start    =         head $ M.keys  $ M.filter isRobot plan
    dir      = toDir $ head $ M.elems $ M.filter isRobot plan

    isRobot  = flip elem [ RobotN, RobotS, RobotW, RobotE ]

    toDir RobotN = north
    toDir RobotS = south
    toDir RobotW = west
    toDir RobotE = east

    -- load our robot with the floor plan and determine the path from start to end
    robot    = Robot plan start dir [] []
    path     = evalState walk robot

    -- turn the list of individual movements into a list of segments.
    -- this is not actually used further; I outputted it during development to get
    -- the segment list so I could manually reason about it (see bottom of file)
    segments = compress path

    -- results of manually sorting the segment list (automation of this step tbd)
    main     = "A,B,A,A,B,C,B,C,C,B"
    a        = "L,12,R,8,L,6,R,8,L,6"
    b        = "R,8,L,12,L,12,R,8"
    c        = "L,6,R,6,L,12"

    -- turn our route into a list of opcodes for the Intcode computer
    program  = map (ord >>> Value) list
    list     = intercalate nl [main,a,b,c,video] ++ nl
    nl       = [chr 10]

    -- we have the option to view a continuous video feed, but we are too cool to need it
    video    = "n"

    -- the second run of the Intcode program will move the robot around the map.
    -- first change the first memory address to 2 to awaken it
    woken    = M.insert (Addr 0) (Value 2) opcodes
    comp'    = initIntcode woken program False

    result   = head $ outputs $ execState step comp'


{- Robot state machine -}

-- Assumptions:
--   The robot always goes straight through an intersection and never turns at one


-- determine the path from the start to the end of the scaffolding
walk :: State Robot [Movement]
walk = go >> reverse <$> gets moves

go :: State Robot ()
go = firstOf [ (ahead, Step     , advance   )     -- first try stepping forward
             , (left , TurnLeft , turn left )     -- then try turning left, right
             , (right, TurnRight, turn right) ]
  where
    firstOf ( (dir,movement,action) : rest ) =

      look dir >>= \case Scaffold -> jot movement
                                       >> action
                                       >> go
                         Empty    -> firstOf rest

    -- if there's nowhere to go, our robot is at the end of the path and we're done
    firstOf [] = return ()


-- step the robot forward (by this point we've determined there's scaffolding there)
advance :: State Robot ()
advance = do
  pos <- gets position
  dir <- gets direction

  let pos' = move pos dir

  modify $ \r -> r { position = pos' }


-- turn left or right
turn :: (Dir -> Dir) -> State Robot ()
turn f = (f <$> gets direction) >>= face

-- face the robot in a given direction
face :: Dir -> State Robot ()
face dir = modify $ \c -> c { direction = dir }

-- jot down the result of one attempt to step in our path-walking
jot :: Movement -> State Robot ()
jot m = modify $ \r -> r { moves = m : moves r }
  
-- look at what's in a given direction (ahead, left, or right)
look :: (Dir -> Dir) -> State Robot Tile
look reldir = do
  plan <- gets floorplan
  pos  <- gets position
  dir  <- reldir <$> gets direction

  let pos' = move pos dir

  case M.lookup pos' plan of
    Nothing       -> return Empty
    Just Empty    -> return Empty
    Just Scaffold -> return Scaffold


-- compress individual movements to their segment representation
--
-- eg: [Left,Step,Step,Right,Step] -> [(L,2),(R,1)]
--
compress :: [Movement] -> [Segment] 
compress moves = go $ group moves
  where
    go []                             = []
    go ( [TurnLeft ] : steps : rest ) = ('L', length steps) : go rest
    go ( [TurnRight] : steps : rest ) = ('R', length steps) : go rest


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

ahead :: Dir -> Dir
ahead = id


{- Rendering -}

render :: M.Map Pos Tile -> String
render screen = unlines [[ point (col,row) | col <- [0..60]]
                                           | row <- [0..62]]
  where
    point pos = case M.lookup pos screen of
                  Nothing       -> ' '
                  Just Empty    -> '.'
                  Just Scaffold -> '#'
                  Just RobotN   -> '^'
                  Just RobotS   -> 'v'
                  Just RobotW   -> '<'
                  Just RobotE   -> '>'


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show


{- jason@ubuntu18d:~/aoc2019$ time stack exec aoc2019-exe
   5068
   1415975

   real    0m0.604s
   user    0m0.581s


   Our puzzle input describes the following map:

   ........................#############........................
   ........................#...........#........................
   ........................#...........#........................
   ........................#...........#........................
   ........................#...#############....................
   ........................#...#.......#...#....................
   ........................#...#.......#...#....................
   ........................#...#.......#...#....................
   ........................#...#.......#######..................
   ........................#...#...........#.#..................
   ......................#######...........#######..............
   ......................#.#.................#...#..............
   ................#########.................#...#..............
   ................#.....#...................#...#..............
   ................#.....#...................#...#..............
   ................#.....#...................#...#..............
   ................#.....#...................#######............
   ................#.....#.......................#.#............
   ................#.....#.......................#############..
   ................#.....#.........................#.........#..
   ................#.....#.........................#.........#..
   ................#.....#.........................#.........#..
   ..............#########.........................#.........#..
   ..............#.#...............................#.........#..
   ........#########...............................############^
   ........#.....#...........................................#..
   ........#.....#...........................................#..
   ........#.....#...........................................#..
   ........#.....#...........................................#..
   ........#.....#...........................................#..
   #########.....#...................................#########..
   #.............#...................................#..........
   #.............#...................................#..........
   #.............#...................................#..........
   #.............#############.......................#..........
   #.........................#.......................#..........
   #############.............#.......................#..........
   ............#.............#.......................#..........
   ............#.............#.......................#..........
   ............#.............#.......................#..........
   ............#.............#.......................#..........
   ............#.............#.......................#..........
   ............#.............#.................#######..........
   ............#...............................#................
   ............#######.........................#................
   ..................#.........................#................
   ..................#.........................#................
   ..................#.........................#................
   ..................#.................#########................
   ..................#.................#........................
   ..................#.................#........................
   ..................#.................#........................
   ..................#######...........#........................
   ........................#...........#........................
   ........................#...........#........................
   ........................#...........#........................
   ........................#...........#........................
   ........................#...........#........................
   ........................#...........#........................
   ........................#...........#........................
   ........................#############........................


   Path segments:
      L12,R8,L6,R8,L6,R8,L12,L12,R8,L12,R8,L6,R8,L6,L12,R8,L6,R8,L6,R8,L12,L12,R8,L6,R6,L12,R8,L12,L12,R8,L6,R6,L12,L6,R6,L12,R8,L12,L12,R8


   Manually sorting them out: [A,B,A,A,B,C,B,C,C,B]

    A) L12,R8,L6,R8,L6
                        B) R8,L12,L12,R8
    A) L12,R8,L6,R8,L6
    A) L12,R8,L6,R8,L6
                        B) R8,L12,L12,R8
                                          C) L6,R6,L12
                        B) R8,L12,L12,R8
                                          C) L6,R6,L12
                                          C) L6,R6,L12
                        B) R8,L12,L12,R8
-}

