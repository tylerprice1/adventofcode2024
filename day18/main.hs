module Main where

import Data.List (find)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace (traceShowId)
import Dijkstra (dijkstra)
import GHC.Base (maxInt)
import Maze (Maze (..))
import Utils (Position)

part1 :: Maze -> Int
part1 input = fst (dijkstra input)

part2 :: (Maze, Int, [Position]) -> Maybe Position
part2 (Maze width height start end _, toDrop, positions) =
  (\(p, i) -> Just p)
    =<< binarySearch
      ( \(p, i) ->
          maxInt == fst (dijkstra (Maze width height start end (Set.fromList (Vector.toList (Vector.take (traceShowId i) positionsVector)))))
      )
      (Vector.fromList (zip positions [1 ..]))
  where
    positionsVector = Vector.fromList positions
    binarySearch :: (a -> Bool) -> Vector.Vector a -> Maybe a
    binarySearch predicate v
      | Vector.null v = Nothing
      | Vector.length v == 1 =
          let first = Vector.head v
           in if predicate first then Just first else Nothing
      | otherwise =
          let half = Vector.length v `div` 2
              curr = (Vector.!) v half
              currResult = predicate curr
              prevResult = predicate ((Vector.!) v (half - 1))
           in ( if currResult
                  then
                    ( if prevResult
                        then binarySearch predicate (Vector.take half v)
                        else Just curr
                    )
                  else binarySearch predicate (Vector.drop half v)
              )

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile
  let (testMaze, _, _) = test

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile
  let (inputMaze, _, _) = input

  putStrLn "\n----- Part 1 -----"
  print (part1 testMaze) -- Expected: ?
  print (part1 inputMaze) -- Expected: ?
  putStrLn "\n----- Part 2 -----"
  print (part2 test) -- Expected: ?
  print (part2 input) -- Expected: ?

processInput :: String -> (Maze, Int, [Position])
processInput contents = (Maze width height (0, 0) (width - 1, height - 1) (Set.fromList (take toTake positions)), toTake, positions)
  where
    lns = lines contents
    (sizeLine : takeLine : rest) = lns

    (width, height) = parseLine sizeLine
    toTake = read takeLine :: Int
    positions = map parseLine rest

    parseLine line =
      let (x, y) = break (== ',') line
       in (read x :: Int, read (tail y) :: Int)
