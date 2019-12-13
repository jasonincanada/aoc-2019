module Main where

import Day13 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/13.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

