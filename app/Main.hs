module Main where

import Day15 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/15.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

