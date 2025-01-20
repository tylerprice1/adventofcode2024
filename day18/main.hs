module Main where

import Data.Set qualified as Set
import Debug.Trace (traceShowId)
import Dijkstra (dijkstra)
import Maze (Maze (..))
import Utils (Position)

part1 input = fst (dijkstra input)

part2 input = input

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: ?
  print (part1 input) -- Expected: ?
  putStrLn "\n----- Part 2 -----"
  print (part2 test) -- Expected: ?
  print (part2 input) -- Expected: ?

processInput :: String -> Maze
processInput contents = Maze width height (0, 0) (width - 1, height - 1) (Set.fromList (take toTake positions))
  where
    lns = lines contents
    (sizeLine : takeLine : rest) = lns

    (width, height) = parseLine sizeLine
    toTake = read takeLine :: Int
    positions = map parseLine rest

    parseLine line =
      let (x, y) = break (== ',') line
       in (read x :: Int, read (tail y) :: Int)
