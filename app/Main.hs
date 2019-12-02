module Main where

import Day02 (part1, part2)

main :: IO ()
main = do
  input <- readFile "inputs/2.input"
  putStrLn $ part1 input
  putStrLn $ part2 input

