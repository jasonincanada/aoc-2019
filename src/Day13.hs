{-# Language LambdaCase, MultiWayIf, ViewPatterns #-}

module Day13 (part1, part2) where

{-  Advent of Code 2019 - Day 13 - https://adventofcode.com/2019/day/13 -}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Bool           (bool)
import qualified Data.Map as M
import           Data.List.Split     (splitOn)

import           Intcode


{- Types -}

data Tile   = Empty
            | Wall
            | Block
            | Paddle
            | Ball
            deriving (Enum, Eq, Show)

type Pos    = (Int, Int)  -- x/y
type Score  = Int

data Arcade = Arcade { screen    :: M.Map Pos Tile
                     , intcode   :: Intcode         -- the embedded Intcode computer
                     , queue     :: [Int]           -- output signals queue up until 3
                     , score     :: Int             -- last known score
                     , paddle    :: Int             -- last drawn paddle offset
                     , ball      :: Int             -- last drawn ball offset
                     }

type Input   = RAM
data Output  = Part1 Int
             | Part2 Int

instance Show Output where
  show (Part1 count) = show count
  show (Part2 score) = show score



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
    comp    = initIntcode opcodes [] True
    arcade  = initArcade comp
    screen  = evalState goarcade arcade
    result  = M.size $ M.filter (==Block) screen


-- initialize an arcade machine with an Intcode computer
initArcade :: Intcode -> Arcade
initArcade comp = Arcade M.empty comp [] 0 0 0


{- Arcade state machine -}

goarcade :: State Arcade (M.Map Pos Tile)
goarcade = do
  comp <- gets intcode

  case runState step comp of
    (SignalOut val, comp') -> do modify (\c -> c { intcode = comp'})
                                 process val

    (Halt, _)              -> gets screen


-- process a signal once we have three of them
process :: Int -> State Arcade (M.Map Pos Tile)
process val = do
  vals <- gets queue

  case vals of
    []    -> addSignal  val >> goarcade
    [_]   -> addSignal  val >> goarcade
    [x,y] -> draw (x,y) val >> clearSignals >> goarcade


addSignal :: Int -> State Arcade ()
addSignal s  = modify $ \c -> c { queue = queue c ++ [s] }
clearSignals = modify $ \c -> c { queue = [] }


-- draw this tile onto the arcade screen (add it to the map)
draw :: Pos -> Int -> State Arcade ()
draw pos (toEnum -> tile) = do
  modify $ \c -> c { screen = M.insert pos tile (screen c) }



{- Part 2 -}

calc2 :: Input -> Output
calc2 opcodes = Part1 score
  where
    cheat   = M.insert (Addr 0) (Value 2) opcodes  -- pop in some quarters
    comp    = initIntcode cheat [] True
    arcade  = initArcade comp
    score   = evalState goarcade2 arcade


{- Arcade state machine -}

-- part 2 uses a different processor to feed inputs to the joystick and to
-- watch the screen for important updates
goarcade2 :: State Arcade Score
goarcade2 = do
  comp <- gets intcode

  case runState step comp of
    (SignalOut val, comp') -> do modify (\c -> c { intcode = comp'})
                                 process2 val

    (Halt, _)              -> gets score


process2 :: Int -> State Arcade Score
process2 val = do
  vals <- gets queue

  case vals of
    []     -> addSignal val >> goarcade2
    [_]    -> addSignal val >> goarcade2

    -- with [-1,0] in the queue, the latest signal is our score
    [-1,0] -> setScore val
                >> clearSignals
                >> goarcade2

    -- update something on the arcade screen
    [x,y]  -> draw (x,y) val
                >> play x val
                >> clearSignals
                >> goarcade2


setScore :: Int -> State Arcade ()
setScore s = modify $ \c -> c { score = s}

play :: Int -> Int -> State Arcade ()
play x (toEnum -> tile)

  -- if we've just drawn the paddle, remember where it is
  | tile == Paddle = modify $ \c -> c { paddle = x }

  -- move the paddle closer to the ball
  | tile == Ball   = do pad <- gets paddle

                        if | pad < x   -> joystick   1    -- right
                           | pad > x   -> joystick (-1)   -- left
                           | otherwise -> joystick   0

  | otherwise      = return ()


-- send a joystick move signal to the embedded computer
joystick :: Int -> State Arcade ()
joystick dir = do
  comp <- sendInput dir <$> gets intcode

  modify $ \c -> c { intcode = comp }


render :: M.Map Pos Tile -> String
render screen = unlines [[ point (col,row) | col <- [0..40]]
                                           | row <- [0..24]]
  where
    point pos = case M.lookup pos screen of
                  Nothing     -> ' '
                  Just Empty  -> ' '
                  Just Wall   -> '#'
                  Just Block  -> '*'
                  Just Paddle -> '_'
                  Just Ball   -> 'O'

{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show


{-  jason@ubuntu18d:~/aoc2019$ time stack exec aoc2019-exe
    230
    11140

    real    0m0.801s
    user    0m0.764s
    sys     0m0.026s


    The start of the game board rendered in ASCII:

    #########################################
    #                                       #
    #     ** * * *  ** *       *       *  * #
    #    *  * **** *   **  ****     *    ** #
    #     *   *      ** *    *    *    ***  #
    #  ***   *     **  * *   **   *   *     #
    #          *   *    *  *    *  * * * *  #
    #     **  **    *  **   **  *   ***** * #
    #    * *  *      *  * *** ***     *  *  #
    # *  *    *    ** *    *     * *  *  *  #
    # ***    ******       *  * * ** ** * *  #
    #  *  * *  *  *   *    *  *  ** *    ** #
    #  **  ** **     *    ***  * * **  * *  #
    #  ***  ** ***   *** *     *  * *       #
    # *   ** *  * ***  ** *      ** ** *  * #
    #  *  **     ** *** * ***  * ***      * #
    # * * ****   *   **    * **    * *  *   #
    #    *  *  ** *   *      *  *         * #
    # *   *   ** *  *  ** *  *        *  *  #
    #                                       #
    #                 O                     #
    #                                       #
    #                                       #
    #                   _                   #
    #                                       #

    Pretty damn cool!
-}

