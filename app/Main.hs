module Main where

import Day05 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/5.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

