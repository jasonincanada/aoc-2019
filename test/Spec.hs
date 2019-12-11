import Control.Monad (forM_)
import Test.Hspec

import qualified Day10 as Day10

main :: IO ()
main = hspec $ do

  describe "Day 10" $ do

    -- make sure our coordinate conversion function operates correctly
    --
    -- Day10.cast maps the AoC coordinate grid to the Cartesian grid such that
    -- the clockwise sweeping of the AoC laser starting from the top is equivalent to
    -- counter-clockwise sweeping on the cartesian plane starting from the left;
    -- ie: ordering by the atan2 angle
    context "cast" $ do

      -- (col, row)
      let cases = [ ((0, 0), ( 0, 0)) -- AoC origin maps to x/y Cartesian origin
                  , ((0,-1), (-1, 0))
                  , ((1, 0), ( 0,-1))
                  , ((0, 1), ( 1, 0))
                  , ((-1,0), ( 0, 1))
                  ]

      forM_ cases $
        \(test, expected) ->
          it (show test ++ " -> " ++ show expected) $
            Day10.cast test `shouldBe` expected

