{-# Language LambdaCase #-}

module Day05 (part1, part2) where

{-  Advent of Code 2019 - Day 5 - https://adventofcode.com/2019/day/5

-}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Bool           (bool)
import qualified Data.IntMap as IM
import           Data.List.Split     (splitOn)


{- Types -}

type Address = Int
type Opcode  = Int
type Input   = IM.IntMap Opcode

data Mode    = Position | Immediate

-- the state of the computer at any time is the instruction pointer
-- and the list of (mutable) opcodes
data Computer = Computer { address :: Address
                         , memory  :: IM.IntMap Opcode
                         , inputs  :: [Int]
                         , outputs :: [Int]
                         , mode1   :: Mode
                         , mode2   :: Mode
                         , mode3   :: Mode
                         }

-- the list of outputs (via opcode 4)
data Output  = Output [Int]

instance Show Output where
  show (Output value) = show value


{- Parsing -}

parse :: String -> Input
parse = split >>> map read >>> toMap
  where
    split = splitOn ","
    toMap = IM.fromList . zip [0..]


{- Part 1 -}

calc1 :: Input -> Output
calc1 opcodes = Output result
  where
    result = evalState process start

    -- the input list is just 1, given in the problem description
    start  = Computer 0 opcodes [1] [] Position Position Position


process :: State Computer [Int]
process = seek 0 >> step


step :: State Computer [Int]
step = do
  ip <- gets address
  instruction <- readAt ip

  let opcode = instruction `mod` 100

  setMode 1 (getDigit 3 instruction == 1)
  setMode 2 (getDigit 4 instruction == 1)

  case opcode of
    1  -> do add1At <- readAt (ip+1)    -- get the locations of our data
             add2At <- readAt (ip+2)
             destAt <- readAt (ip+3)

             -- get the actual values to add
             add1   <- getValue 1 add1At
             add2   <- getValue 2 add2At

             update destAt (add1+add2)
             seek (ip+4)

             step

    2  -> do mul1At <- readAt (ip+1)    -- get the locations of our data
             mul2At <- readAt (ip+2)
             destAt <- readAt (ip+3)

             -- get the actual values to multiply
             mul1   <- getValue 1 mul1At
             mul2   <- getValue 2 mul2At

             update destAt (mul1*mul2)
             seek (ip+4)

             step

    3  -> do int    <- input
             destAt <- readAt (ip+1)

             update destAt int
             seek (ip+2)

             step

    4  -> do at <- readAt (ip+1)

             outputAt at
             seek (ip+2)

             step

    -- jump-if-true
    5  -> do param1 <- readAt (ip+1) >>= getValue 1

             if param1 /= 0
             then do param2 <- readAt (ip+2) >>= getValue 2
                     seek param2
                     step

             else do seek (ip+3)
                     step

    -- jump-if-false
    6  -> do param1 <- readAt (ip+1) >>= getValue 1

             if param1 == 0
             then do param2 <- readAt (ip+2) >>= getValue 2
                     seek param2
                     step

             else do seek (ip+3)
                     step

    -- less than
    7  -> do param1 <- readAt (ip+1) >>= getValue 1
             param2 <- readAt (ip+2) >>= getValue 2
             param3 <- readAt (ip+3) >>= getValue 3

             update param3 (bool 0 1 $ param1 < param2)
             seek (ip+4)

             step

    -- equals
    8  -> do param1 <- readAt (ip+1) >>= getValue 1
             param2 <- readAt (ip+2) >>= getValue 2
             param3 <- readAt (ip+3) >>= getValue 3

             update param3 (bool 0 1 $ param1 == param2)
             seek (ip+4)

             step


    99 -> reverse <$> gets outputs


-- get the nth digit from the right of a base-10 integer
getDigit :: Int -> Int -> Int
getDigit i number = number `div` (10^(i-1)) `mod` 10

-- move the instruction pointer
seek :: Address -> State Computer ()
seek addy = modify (\c -> c { address = addy } )

-- get an opcode at an address
readAt :: Address -> State Computer Opcode
readAt addy = gets (memory >>> flip (IM.!) addy)

-- in Immediate mode, return the value
-- in Position mode, consider it an address and return what's at the address
getValue :: Int -> Int -> State Computer Int
getValue n val
  | n == 1 = go mode1
  | n == 2 = go mode2
  | n == 3 = go mode3
  where
    go m = gets m >>= \case Position  -> readAt val
                            Immediate -> return val

-- change an opcode at a position
update :: Address -> Int -> State Computer ()
update addy opcode = do
  memory' <- IM.insert addy opcode <$> gets memory
  modify (\c -> c { memory = memory' })

-- get a value from the input
input :: State Computer Int
input = do
  (i:is) <- gets inputs
  modify (\c -> c { inputs = is })
  return i

outputAt :: Address -> State Computer ()
outputAt addy = do
  value   <- readAt addy
  outputs <- gets outputs
  modify (\c -> c { outputs = value : outputs })

setMode :: Int -> Bool -> State Computer ()
setMode 1 mode = modify (\c -> c { mode1 = bool Position Immediate mode })
setMode 2 mode = modify (\c -> c { mode2 = bool Position Immediate mode })
setMode 3 mode = modify (\c -> c { mode3 = bool Position Immediate mode })


{- Part 2 -}

calc2 :: Input -> Output
calc2 opcodes = Output result
  where
    result = evalState process start

    -- the input list is just 5, given in the problem description
    start  = Computer 0 opcodes [5] [] Position Position Position


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

