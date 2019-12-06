module Common (
  swap,
  commonPrefix,
  unique
  ) where

import Control.Arrow   ((>>>))
import Data.List       (group, sort)


-- get the longest common prefix of two lists
commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix [] _  = []
commonPrefix _ []  = []
commonPrefix (s:ss) (t:tt)
  | s == t    = s : commonPrefix ss tt
  | otherwise = []


-- swap the elements of a pair
swap :: (a, b) -> (b, a)
swap (a,b) = (b,a)


-- like Data.List.nub but cooler
unique :: Ord a => [a] -> [a]
unique = sort >>> group >>> map head

