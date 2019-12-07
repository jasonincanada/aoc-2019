module Main where

import Day07 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/7.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

