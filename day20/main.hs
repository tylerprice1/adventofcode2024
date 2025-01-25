{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module Main (main) where

import Control.DeepSeq (deepseq)
import Control.Parallel.Strategies (parBuffer, parListChunk, rdeepseq, using)
import Data.List (group, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (Distance, dijkstra)
import GHC.Base (maxInt)
import Maze (Maze (..), getBorders, getNonBorders, getNonWalls, getPositions, readMaze, showMazeWithPath)
import Utils (Position)

-- | 2-tuple of Position representing (start, end)
type Cheat = (Position, Position)

race :: Maze -> Maze -> Cheat -> Int -> Maybe Distance
{-# INLINEABLE race #-}
race (Maze width height start end walls nonWalls) (Maze _ _ _ _ cheatWalls cheatNonWalls) (cheatStart, cheatEnd) maxCheatDuration =
  let (!toCheatEnd, _) = dijkstra (Maze width height cheatStart cheatEnd cheatWalls cheatNonWalls) maxCheatDuration -- no walls
   in if toCheatEnd <= maxCheatDuration
        then
          let (!toCheatStart, _) = dijkstra (Maze width height start cheatStart walls nonWalls) maxInt
              (!toEnd, _) = dijkstra (Maze width height cheatEnd end walls nonWalls) maxInt
           in Just (toCheatStart + toCheatEnd + toEnd)
        else
          Nothing

part1 :: (Maze, Distance, [Cheat]) -> Int
part1 (maze, cheatless, cheats) =
  let (Maze width height start end walls _) = maze
      borders = Set.fromList (getBorders maze)
      nonBorders = Set.fromList (getNonBorders maze)
      cheatMaze = Maze width height start end borders nonBorders

      possibleCheats = filter (\((x1, y1), (x2, y2)) -> abs (x2 - x1) <= 2 && abs (y2 - y1) <= 2) cheats
      cheated = concatMap catMaybes (map (map (\c -> race maze cheatMaze c 2)) (chunksOf 10000 possibleCheats) `using` parBuffer 7 rdeepseq)

      saved = map (cheatless -) cheated
   in length (filter (>= 100) saved)

part2 :: (Maze, Distance, [Cheat]) -> Int
part2 (maze, cheatless, cheats) =
  let (Maze width height start end walls _) = maze
      borders = Set.fromList (getBorders maze)
      nonBorders = Set.fromList (getNonBorders maze)
      cheatMaze = Maze width height start end borders nonBorders

      possibleCheats = filter (\((x1, y1), (x2, y2)) -> abs (x2 - x1) <= 20 && abs (y2 - y1) <= 20) cheats
      cheated = concatMap (filter (> 0) . catMaybes) (map (map (\c -> race maze cheatMaze c 20)) (chunksOf 100000 possibleCheats) `using` parBuffer 7 rdeepseq)

      saved = map (cheatless -) (cheated)
   in length ((filter (>= 100) saved))

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let !input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  -- print (part1 test) -- Expected: ?
  -- print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  print (part2 input) -- Expected: ?

processInput :: String -> (Maze, Int, [Cheat])
processInput contents = (maze, cheatlessDistance, cheats)
  where
    maze = readMaze contents
    !nonWalls = getNonWalls maze
    (!cheatlessDistance, _) = traceShow (length nonWalls, (length nonWalls) ^ 2) (dijkstra maze maxInt)

    cheats = foldl (\acc p1 -> foldl (\pairs p2 -> (p1, p2) : pairs) acc nonWalls) [] nonWalls
