{-# Language LambdaCase, MultiWayIf #-}

module Day07 (part1, part2) where

{-  Advent of Code 2019 - Day 7 - https://adventofcode.com/2019/day/7 -}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Function       ((&))
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import           Data.List           (permutations)
import           Data.List.Split     (splitOn)
import           Intcode


{- Types -}

type Input   = RAM
data Output  = Output Int

instance Show Output where
  show (Output value) = show value


{- Parsing -}

parse :: String -> Input
parse = split >>> map (read >>> Value) >>> toMap
  where
    split = splitOn ","
    toMap = M.fromList . zip (Addr <$> [0..])


{- Part 1 -}

type Phase  = Int  -- phase setting
type Signal = Int  -- amp input or output signal

calc1 :: Input -> Output
calc1 opcodes = Output result
  where
    result = maximum [ try ps | ps <- permutations [0,1,2,3,4] ]

    try :: [Phase] -> Signal
    try phases = foldl run 0 phases
      where
        run :: Signal -> Phase -> Signal
        run s p = case runState step start of
                    (Halt, amp) -> head (outputs amp)
          where
            start = initIntcode opcodes [Value p, Value s] False

    -- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b



{- Part 2 -}

calc2 :: Input -> Output
calc2 opcodes = Output result
  where
    result = maximum [ try ps | ps <- permutations [5,6,7,8,9] ]

    try :: [Phase] -> Signal
    try phases = go amps 0  -- start with the first amp
                         0  -- the first input signal is 0
      where
        go :: IM.IntMap Intcode -> Int -> Signal -> Signal
        go amps i signal =

            case result of
              SignalOut sig -> go amps' (next i) sig
              Halt          -> head (outputs $ amps' IM.! 4) -- last output from amp E

          where
            (result, state) = runState (stepInput signal)
                                       (amps IM.! i)

            -- save this state to the map so we can resume from here next time
            amps' = IM.insert i state amps

            -- cycle to the next amp, wrapping from E back to A
            next 4 = 0
            next n = n+1


        -- init each amp with its phase setting
        amps :: IM.IntMap Intcode
        amps = IM.fromList $ zip [ 0.. ]
                                 [ start p | p <- phases ]

        -- load up the phase setting as its first input
        start phase = initIntcode opcodes [Value phase] True 



{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

