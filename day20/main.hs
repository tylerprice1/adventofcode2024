module Main (main) where

import Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import Data.List (sort)
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
{-# INLINE race #-}
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

      cheated = filter (< cheatless) (catMaybes (map (\c -> race maze cheatMaze c 2) cheats `using` parBuffer 7 rdeepseq))

      saved = map (cheatless -) cheated
   in length (filter (>= 100) (traceShow (sort cheated, sort saved) saved))

part2 :: (Maze, [Cheat]) -> Int
part2 input = 0

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let !input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  -- print (part1 test) -- Expected: ?
  print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?

processInput :: String -> (Maze, Int, [Cheat])
processInput contents = (maze, traceShow (cheatlessDistance, cheatlessDistance ^ 2) cheatlessDistance, cheats)
  where
    maze = readMaze contents
    (cheatlessDistance, cheatlessPath) = dijkstra maze maxInt

    cheats = foldl (\acc p1 -> foldl (\pairs p2 -> (p1, p2) : pairs) acc cheatlessPath) [] cheatlessPath
