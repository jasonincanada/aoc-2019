{-# Language LambdaCase, MultiWayIf #-}

module Day09 (part1, part2) where

{-  Advent of Code 2019 - Day 9 - https://adventofcode.com/2019/day/9 -}

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
                         , base    :: Int              -- relative base
                         , mode1   :: Mode             -- modes set by latest instruction
                         , mode2   :: Mode
                         , mode3   :: Mode
                         , inputs  :: [Value]          -- list of inputs for opcode 3
                         , outputs :: [Int]            -- outputs generated by opcode 4
                         }

type Input   = RAM
data Output  = Output [Int]

instance Show Output where
  show (Output value) = show value

instance Show Value where
  show (Value v) = show v


{- Parsing -}

parse :: String -> Input
parse = split >>> map (read >>> Value) >>> toMap
  where
    split = splitOn ","
    toMap = M.fromList . zip (Addr <$> [0..])


-- init a computer with a given program and list of inputs
initComp :: RAM -> [Value] -> Computer
initComp ram inputs = Computer (Addr 0)
                               ram
                               0
                               Position
                               Position
                               Position
                               inputs
                               []


{- Part 1 -}

calc1 :: Input -> Output
calc1 opcodes = Output result
  where
    result = evalState step start
    start  = initComp opcodes [Value 1]  -- 1 is the starting input given by the problem


step :: State Computer [Int]
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
    4  -> do param 1 >>= output

             jump 2
             step


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
    99 -> reverse <$> gets outputs


-- get the nth digit from the right of a base-10 integer
getDigit :: Int -> Int -> Int
getDigit i number = number `div` (10^(i-1)) `mod` 10

jump :: Int -> State Computer ()
jump n = do
  Addr ip <- gets address
  seek (Addr $ ip+n)

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
adjustBase (Value n) = modify (\c -> c { base = base c + n })

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
                                Relative  -> gets base >>= ((+val) >>> Addr >>> readAt)


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
                                Relative  -> gets base >>= ((+val) >>> Addr >>> return)


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
calc2 opcodes = Output result
  where
    result = evalState step start
    start  = initComp opcodes [Value 2]  -- 2 is the starting input given by the problem


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show


{-  jason@ubuntu18d:~/aoc2019$ time stack exec aoc2019-exe
    [2453265701]
    [80805]

    real    0m0.493s
    user    0m0.470s
    sys     0m0.004s
-}

