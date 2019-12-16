module Dijkstra (dijkstra) where

{- Dijkstra's shortest path algo -}

import Data.Map.Strict hiding (null)

dijkstra :: Ord a => a -> [(a,a)] -> (Map a Int, Int)
dijkstra from edges = go edges explored 0
  where
    explored = singleton from 0

    go :: Ord a => [(a,a)] -> Map a Int -> Int -> (Map a Int, Int)
    go edges explored count
      | null frontier = (explored, count)
      | otherwise     = go edges new (count+1)
      where
        frontier = [ (from,to) | (from,to) <- edges,
                                 from `member`    explored,
                                 to   `notMember` explored ]

        new = union explored $ fromList [ (to, dist+1) 
                                          | (from,to) <- frontier,
                                            let dist = explored ! from ]

