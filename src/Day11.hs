{-# Language LambdaCase, MultiWayIf #-}

module Day11 (part1, part2) where

{-  Advent of Code 2019 - Day 11 - https://adventofcode.com/2019/day/11 -}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Bool           (bool)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List.Split     (splitOn)

import           Intcode


{- Types -}

-- the state of the paint robot at any time
data Robot = Robot { panels    :: M.Map Pos Color  -- map of known panel colors
                   , painted   :: S.Set Pos        -- any tile that's been painted
                   , position  :: Pos
                   , direction :: Dir
                   , intcode   :: Intcode          -- encapsulate the computer's state
                   , queue     :: [Int]            -- output signals queue up until 2
                   }

type Input   = RAM
data Output  = Part1 Int
             | Part2 [Pos]

instance Show Output where
  show (Part1 count) = show count
  show (Part2 poses) = draw poses


{- Parsing -}

parse :: String -> Input
parse = split >>> map (read >>> Value) >>> toMap
  where
    split = splitOn ","
    toMap = M.fromList . zip (Addr <$> [0..])


{- Part 1 -}

data Color   = Black | White
               deriving Eq

type Pos     = (Int, Int)  -- x/y
type Dir     = (Int, Int)  -- x/y


calc1 :: Input -> Output
calc1 opcodes = Part1 result
  where
    comp    = initIntcode opcodes [Value 0] True
    robot   = initRobot comp
    painted = evalState gorobot robot
    result  = S.size painted


initRobot :: Intcode -> Robot
initRobot comp = Robot M.empty S.empty (0,0) (0,-1) comp []


{- Robot state machine -}

gorobot :: State Robot (S.Set Pos)
gorobot = do
  comp <- gets intcode

  case runState step comp of
    (SignalOut val, comp') -> do modify (\c -> c { intcode = comp'})
                                 process val

    (Halt, _)              -> gets painted


-- process a signal once we have two of them
process :: Int -> State Robot (S.Set Pos)
process val = do
  vals <- gets queue

  case vals of
    []  -> addSignal val
             >> gorobot

    [v] -> paint v
             >> turn val
             >> advance
             >> clearSignals
             >> see
             >> gorobot
      

addSignal :: Int -> State Robot ()
addSignal s  = modify $ \c -> c { queue = queue c ++ [s] }
clearSignals = modify $ \c -> c { queue = [] }


-- paint the panel we're over
paint :: Int -> State Robot ()
paint color = do
  pos <- gets position

  let clr = [Black, White] !! color

  modify $ \c -> c { panels  = M.insert pos clr (panels  c) }
  modify $ \c -> c { painted = S.insert pos     (painted c) }


turn :: Int -> State Robot ()
turn dir = do
  (x,y) <- gets direction

  let dir' = if | dir == 0 -> (y, -x)
                | dir == 1 -> (-y, x)

  modify $ \c -> c { direction = dir' }


-- advance the robot in its current direction by one unit
advance :: State Robot ()
advance = do
  (x, y ) <- gets position
  (dx,dy) <- gets direction

  let pos' = (x+dx, y+dy)

  modify $ \c -> c { position = pos' }


-- look at the color of panel we're over and load it as an input to the computer
see :: State Robot ()
see = do
  pos    <- gets position
  panels <- gets panels
  comp   <- gets intcode

  let input = case M.lookup pos panels of
                Nothing    -> 0
                Just Black -> 0
                Just White -> 1

  let comp' = sendInput input comp

  modify $ \c -> c { intcode = comp' }


{- Part 2 -}

calc2 :: Input -> Output
calc2 opcodes = Part2 result
  where
    comp    = initIntcode opcodes [Value 1] True
    robot   = initRobot comp
    ship    = panels $ execState gorobot robot
    result  = M.keys $ M.filter (==White) ship


draw :: [Pos] -> String
draw poses = unlines [ unwords [ point col row | col <- [0..40]]
                                               | row <- [0..5 ]]
  where
    point col row
      | (col,row) `elem` poses = "#"
      | otherwise              = "."


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show


{-  jason@ubuntu18d:~/aoc2019$ time stack exec aoc2019-exe
    1909
    . . . # # . # . . # . # # # # . # # # # . # . . # . # . . # . # # # . . # . . # .
    . . . . # . # . . # . # . . . . # . . . . # . # . . # . . # . # . . # . # . . # .
    . . . . # . # . . # . # # # . . # # # . . # # . . . # # # # . # . . # . # # # # .
    . . . . # . # . . # . # . . . . # . . . . # . # . . # . . # . # # # . . # . . # .
    . # . . # . # . . # . # . . . . # . . . . # . # . . # . . # . # . . . . # . . # .
    . . # # . . . # # . . # . . . . # # # # . # . . # . # . . # . # . . . . # . . # .


    real    0m0.422s
    user    0m0.393s
    sys     0m0.017s
-}

