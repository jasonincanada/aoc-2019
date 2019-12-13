{-# Language LambdaCase, MultiWayIf, ViewPatterns #-}

module Day13 (part1, part2) where

{-  Advent of Code 2019 - Day 13 - https://adventofcode.com/2019/day/13 -}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Bool           (bool)
import qualified Data.Map as M
import           Data.List.Split     (splitOn)


{- Types -}

data Address = Addr Int
               deriving (Eq, Ord)

-- interpretable as either an integer value or an opcode
data Value   = Value Int

data Mode    = Position | Relative | Immediate
type RAM     = M.Map Address Value

-- the state of the computer at any time
data Computer = Computer { address :: Address          -- instruction pointer
                         , memory  :: RAM
                         , base    :: Address          -- relative base
                         , mode1   :: Mode             -- modes set by latest instruction
                         , mode2   :: Mode
                         , mode3   :: Mode
                         , inputs  :: [Value]          -- list of inputs for opcode 3
                         , outputs :: [Int]            -- outputs generated by opcode 4
                         , signal  :: Bool             -- return from step with signal?
                         }

data Tile   = Empty
            | Wall
            | Block
            | Paddle
            | Ball
            deriving (Enum, Eq, Show)

type Pos    = (Int, Int)  -- x/y
type Score  = Int

data Arcade = Arcade { screen    :: M.Map Pos Tile
                     , computer  :: Computer        -- the embedded Intcode computer
                     , queue     :: [Int]           -- output signals queue up until 3
                     , score     :: Int             -- last known score
                     , paddle    :: Int             -- last drawn paddle offset
                     , ball      :: Int             -- last drawn ball offset
                     }

data StepResult = SignalOut Int   -- the program has output a value
                | Halt            -- a 99 opcode stopped the program

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
    comp    = initComp opcodes [] True
    arcade  = initArcade comp
    screen  = evalState goarcade arcade
    result  = M.size $ M.filter (==Block) screen


-- initialize a computer with a given program and list of inputs
initComp :: RAM -> [Value] -> Bool -> Computer
initComp ram inputs signals = Computer (Addr 0)
                                       ram
                                       (Addr 0)
                                       Position
                                       Position
                                       Position
                                       inputs
                                       []
                                       signals


-- initialize an arcade machine
initArcade :: Computer -> Arcade
initArcade comp = Arcade M.empty comp [] 0 0 0


{- Arcade state machine -}

goarcade :: State Arcade (M.Map Pos Tile)
goarcade = do
  comp <- gets computer

  case runState step comp of
    (SignalOut val, comp') -> do modify (\c -> c { computer = comp'})
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



{- Intcode state machine -}

step :: State Computer StepResult
step = do
  ip <- gets address
  Value instruction <- readAt ip

  let opcode = instruction `mod` 100

  setMode 1 (getDigit 3 instruction)
  setMode 2 (getDigit 4 instruction)
  setMode 3 (getDigit 5 instruction)

  case opcode of

    -- add
    1  -> do Value add1 <- param 1
             Value add2 <- param 2
             addr       <- dest  3

             update addr (Value $ add1+add2)

             jump 4
             step


    -- multiply
    2  -> do Value mul1 <- param 1
             Value mul2 <- param 2
             addr       <- dest  3

             update addr (Value $ mul1*mul2)

             jump 4
             step


    -- save input
    3  -> do val  <- input
             addr <- dest 1

             update addr val

             jump 2
             step


    -- output
    4  -> do Value val <- param 1
             signals   <- gets signal

             output (Value val)

             jump 2

             if | signals   -> return $ SignalOut val
                | otherwise -> step


    -- jump-if-true
    5  -> do Value p1 <- param 1

             if | p1 /= 0   -> do param 2 >>= (toAddr >>> seek)
                                  step
                | otherwise -> do jump 3
                                  step


    -- jump-if-false
    6  -> do Value p1 <- param 1

             if | p1 == 0   -> do param 2 >>= (toAddr >>> seek)
                                  step
                | otherwise -> do jump 3
                                  step


    -- less than
    7  -> do Value p1 <- param 1
             Value p2 <- param 2
             addr     <- dest  3

             update addr (Value $ bool 0 1 (p1 < p2))

             jump 4
             step


    -- equals
    8  -> do Value p1 <- param 1
             Value p2 <- param 2
             addr     <- dest  3

             update addr (Value $ bool 0 1 (p1 == p2))

             jump 4
             step


    -- adjust relative base
    9  -> do param 1 >>= adjustBase

             jump 2
             step


    -- quit
    99 -> return Halt


-- get the nth digit from the right of a base-10 integer
getDigit :: Int -> Int -> Int
getDigit i number = number `div` (10^(i-1)) `mod` 10

jump :: Int -> State Computer ()
jump n = do
  ip <- gets address
  seek (hop n ip)

-- adjust an address by the given number of steps
hop :: Int -> Address -> Address
hop n (Addr a) = Addr (n+a)

toAddr :: Value -> Address
toAddr (Value a) = Addr a

-- move the instruction pointer
seek :: Address -> State Computer ()
seek a = modify (\c -> c { address = a } )

-- adjust the relative base
adjustBase :: Value -> State Computer ()
adjustBase (Value n) = modify (\c -> c { base = hop n (base c) })

-- get an opcode at an address
readAt :: Address -> State Computer Value
readAt a = do
  memory <- gets memory

  case M.lookup a memory of
    Nothing -> return $ Value 0
    Just op -> return op

-- get a parameter value (which can be used in addition, mult, etc)
param :: Int -> State Computer Value
param i = do
  ip <- gets address
  readAt (hop i ip) >>= getValue i

  where
    -- in Immediate mode, return the value
    -- in Position mode, consider it an address and return what's at the address
    -- in Relative mode, consider it an address and return what's at the address+base
    getValue :: Int -> Value -> State Computer Value
    getValue n (Value val)
      | n == 1 = go mode1
      | n == 2 = go mode2
      | n == 3 = go mode3
      where
        go :: (Computer -> Mode) -> State Computer Value
        go m = gets m >>= \case Immediate -> return $ Value val
                                Position  -> readAt $ Addr val
                                Relative  -> gets base >>= (hop val >>> readAt)


-- get a parameter representing a destination address for writing by the calling code
dest :: Int -> State Computer Address
dest i = do
  ip <- gets address
  readAt (hop i ip) >>= getValue i

  where
    -- in Position mode, consider it an address and return it
    -- in Relative mode, consider it a relative address and return it+base
    getValue :: Int -> Value -> State Computer Address
    getValue n (Value val)
      | n == 1 = go mode1
      | n == 2 = go mode2
      | n == 3 = go mode3
      where
        go :: (Computer -> Mode) -> State Computer Address
        go m = gets m >>= \case Position  -> return $ Addr val
                                Relative  -> gets base >>= (hop val >>> return)


-- change an opcode at a position
update :: Address -> Value -> State Computer ()
update a v = do
  memory' <- M.insert a v <$> gets memory
  modify (\c -> c { memory = memory' })

-- get a value from the input
input :: State Computer Value
input = do
  (i:is) <- gets inputs
  modify (\c -> c { inputs = is })
  return i

-- add a value to the input queue
addInput :: Int -> State Computer ()
addInput i = modify (\c -> c { inputs = inputs c ++ [Value i] } )

output :: Value -> State Computer ()
output (Value v) = do
  outputs <- gets outputs
  modify (\c -> c { outputs = v : outputs })

setMode :: Int -> Int -> State Computer ()
setMode 1 mode = modify (\c -> c { mode1 = toMode mode })
setMode 2 mode = modify (\c -> c { mode2 = toMode mode })
setMode 3 mode = modify (\c -> c { mode3 = toMode mode })

toMode 0 = Position
toMode 1 = Immediate
toMode 2 = Relative


{- Part 2 -}

calc2 :: Input -> Output
calc2 opcodes = Part1 score
  where
    cheat   = M.insert (Addr 0) (Value 2) opcodes  -- pop in some quarters
    comp    = initComp cheat [] True
    arcade  = initArcade comp
    score   = evalState goarcade2 arcade


{- Arcade state machine -}

-- part 2 uses a different processor to feed inputs to the joystick and to
-- watch the screen for important updates
goarcade2 :: State Arcade Score
goarcade2 = do
  comp <- gets computer

  case runState step comp of
    (SignalOut val, comp') -> do modify (\c -> c { computer = comp'})
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
  comp <- gets computer

  let comp' = execState (addInput dir) comp

  modify $ \c -> c { computer = comp' }


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
