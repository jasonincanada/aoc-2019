{-# Language ViewPatterns #-}

module Day06 (part1, part2) where

{-  Advent of Code 2019 - Day 6 - https://adventofcode.com/2019/day/6 -}

import Common          (commonPrefix, swap, unique)
import Control.Arrow   ((>>>))
import Data.List.Split (splitOn)
import qualified Data.Map   as M
import qualified Data.Array as A


{- Types -}

type Node     = String
type Edge     = (Node, Node)
type Input    = [Edge]

data Output   = Output Int

instance Show Output where
  show (Output value) = show value


{- Parsing -}

parse :: String -> Input
parse = lines >>> map (splitOn ")" >>> toTuple)
  where
    toTuple [from,to] = (from, to)



{- Part 1 -}

type Hops = Int   -- number of jumps from a node to the root "COM"
type Idx  = Int

calc1 :: Input -> Output
calc1 edges = Output result
  where
    nodes  = nodeList edges

    -- the result for part 1 is simply the sum of the path lengths in array
    result = sum array

    -- a simple array of integers (path lengths) constructed by calling dist--which
    -- refers back to this array, which again calls dist...
    array :: A.Array Idx Hops
    array  = A.listArray bounds [ dist idx | idx <- A.range bounds ]
      where
        bounds = (0, length nodes - 1)

    -- the distance from a node to the root (COM) is 1 more than the node's parent.
    -- COM is 0 hops from itself and forms the base case of our recursion
    dist :: Idx -> Int
    dist ((M.!) idxToNode -> node)
      | node == "COM" = 0
      | otherwise     = 1 + array A.! (nodeToIdx M.! (parents M.! node))


    -- 3 maps instead of pulling in an industrial strength graph library
    idxToNode = M.fromList $ zip [0..] nodes  -- M.Map Idx Node
    nodeToIdx = M.fromList $ zip nodes [0..]  -- M.Map Node Idx
    parents   = M.fromList $ map swap edges   -- M.Map Node Node


    -- get a list of nodes represented in a list of edges
    nodeList :: [Edge] -> [Node]
    nodeList edges = unique (sources ++ targets)
      where
        sources = map fst edges
        targets = map snd edges


{- Part 2 -}

calc2 :: Input -> Output
calc2 edges = Output result
  where
    result  = (length fromYou - length common)
            + (length fromSan - length common)

    fromYou = pathToRoot "YOU"
    fromSan = pathToRoot "SAN"
    common  = commonPrefix fromYou fromSan

    -- get the list of nodes from the given one to the root node COM.
    -- we'll do this for both YOU and SAN, snip out the common prefix, and
    -- add the lengths of the remaining paths to get our result
    pathToRoot :: Node -> [Node]
    pathToRoot = go >>> reverse
      where
        go :: Node -> [Node]
        go ((M.!) parents -> parent)
          | parent == "COM" = [parent]
          | otherwise       =  parent : go parent

        parents = M.fromList $ map swap edges   -- M.Map Node Node


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

