module Main where

import Day17 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/17.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

