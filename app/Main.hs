module Main where

import Day04 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/4.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

