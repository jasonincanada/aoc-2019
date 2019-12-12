module Main where

import Day11 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/11.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

