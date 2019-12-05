module Day05 (part1, part2) where

{-  Advent of Code 2019 - Day 5 - https://adventofcode.com/2019/day/5

-}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import qualified Data.IntMap as IM
import           Data.List.Split     (splitOn)


{- Types -}

type Address = Int
type Opcode  = Int
type Input   = IM.IntMap Opcode

-- the state of the computer at any time is the instruction pointer
-- and the list of (mutable) opcodes
data Computer = Computer { address :: Address
                         , memory  :: IM.IntMap Opcode
                         }

data Output  = Output Int

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
    result = evalState (process 12 2) start
    start  = Computer 0 opcodes


process :: Opcode -> Opcode -> State Computer Opcode
process noun verb = do
  update 1 noun
  update 2 verb
  seek 0

  step


step :: State Computer Opcode
step = do
  ip <- gets address
  opcode <- readAt ip

  case opcode of
    1  -> do add1At <- readAt (ip+1)    -- get the locations of our data
             add2At <- readAt (ip+2)
             destAt <- readAt (ip+3)

             add1   <- readAt add1At    -- get the actual values to add
             add2   <- readAt add2At

             update destAt (add1+add2)
             seek (ip+4)

             step

    2  -> do mul1At <- readAt (ip+1)    -- get the locations of our data
             mul2At <- readAt (ip+2)
             destAt <- readAt (ip+3)

             mul1   <- readAt mul1At    -- get the actual values to multiply
             mul2   <- readAt mul2At

             update destAt (mul1*mul2)
             seek (ip+4)

             step

    99 -> readAt 0


-- move the instruction pointer
seek :: Address -> State Computer ()
seek addy = modify (\c -> c { address = addy } )

-- get an opcode at an address
readAt :: Address -> State Computer Opcode
readAt addy = gets (memory >>> flip (IM.!) addy)

-- change an opcode at a position
update :: Address -> Int -> State Computer ()
update addy opcode = do
  memory' <- IM.insert addy opcode <$> gets memory
  modify (\c -> c { memory = memory' })



{- Part 2 -}

calc2 :: Input -> Output
calc2 opcodes = Output result
  where
    result = head list

    list   = [ 100*noun + verb | noun <- [0..99],
                                 verb <- [0..99],

                                 -- look for this specific number from the description
                                 evalState (process noun verb) start == 19690720 ]
    start  = Computer 0 opcodes


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

