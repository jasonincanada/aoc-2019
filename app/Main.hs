module Main where

import Day08 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/8.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

