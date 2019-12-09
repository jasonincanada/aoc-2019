module Main where

import Day09 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/9.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

