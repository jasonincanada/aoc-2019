module Day03 (part1, part2) where

{-  Advent of Code 2019 - Day 3 - https://adventofcode.com/2019/day/3

-}

import Control.Arrow       ((>>>))
import Control.Monad.State
import Data.List.Split     (splitOn)
import qualified Data.Set        as S
import qualified Data.Map.Strict as M
import Scanner


{- Types -}

type Direction = Char
type Distance  = Int
type Coord     = (Int, Int)
type Path      = [(Direction, Distance)]

type Input     = (Path, Path)
data Output    = Output Int

instance Show Output where
  show (Output distance) = show distance


{- Parsing -}

parse :: String -> Input
parse = lines >>> map toPath >>> toTuple
  where
    toPath          = splitOn "," >>> map p
    p (dir:dist)    = (dir, read dist)
    toTuple (a:b:_) = (a, b)


{- Methods -}

data Hike = Hike { notches :: M.Map Coord Int -- step count to first hit a coord
                 , at      :: Coord           -- current location
                 , steps   :: Int             -- total steps taken
                 }

hike :: Path -> Hike
hike path = execState (walk expanded) start
  where
    start    = Hike M.empty (0,0) 0
    expanded = concatMap exp path

    -- map a leg of the walk to its individual steps
    -- R4 -> [R,R,R,R]
    exp :: (Direction, Distance) -> [Direction]
    exp (dir, dist) = replicate dist dir

    walk :: [Direction] -> State Hike ()
    walk []     = return ()
    walk (d:ds) = step d >> walk ds

    step :: Direction -> State Hike ()
    step direction = do
      notches <- gets notches
      (x, y)  <- gets at
      steps   <- gets steps

      let coord = case direction of
                    'U' -> (x  , y+1)
                    'R' -> (x+1, y  )
                    'D' -> (x  , y-1)
                    'L' -> (x-1, y  )

      -- only record the first time we've hit a coord
      let notches' = if coord `M.member` notches
                     then notches
                     else M.insert coord (steps+1) notches

      put $ Hike notches' coord (steps+1)


coords :: Hike -> [Coord]
coords hike = M.keys (notches hike)


{- Part 1 -}

calc1 :: Input -> Output
calc1 (left, right) = Output result
  where
    leftHike  = hike left
    rightHike = hike right

    -- put the walked coords into sets to deduplicate them and so we can take their
    -- intersection
    leftSet  = S.fromList $ coords leftHike
    rightSet = S.fromList $ coords rightHike
    inters   = S.intersection leftSet rightSet
    result   = minimum $ map manhattan $ S.toList inters

    -- all calculations are relative to the origin (0,0) so we only need one coord
    manhattan :: Coord -> Int
    manhattan (x,y) = abs x + abs y


calc2 :: Input -> Output
calc2 (left, right) = Output result
  where
    leftHike  = hike left
    rightHike = hike right

    leftSet  = S.fromList $ coords leftHike
    rightSet = S.fromList $ coords rightHike
    inters   = S.toList $ S.intersection leftSet rightSet

    sums     = map (add leftHike rightHike) inters

    add :: Hike -> Hike -> Coord -> Int
    add left right coord = (notches left ) M.! coord
                         + (notches right) M.! coord

    result = minimum sums


{- Operations -}

part1 :: String -> String
part1 = parse >>> calc1 >>> show
part2 = parse >>> calc2 >>> show

