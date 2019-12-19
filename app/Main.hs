module Main where

import Day19 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/19.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

