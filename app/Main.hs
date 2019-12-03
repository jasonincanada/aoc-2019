module Main where

import Day03 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/3.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

