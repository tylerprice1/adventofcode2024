module Main (main) where

import Control.Parallel.Strategies (parBuffer, parList, parListChunk, parMap, rdeepseq, rpar, using)
-- import Debug.Trace (trace, traceShow, traceShowId)

import Data.List (group, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (dijkstra)
import Maze (Maze (..), getBorders, getNonWalls, showMazeWithPath)
import Text.Show.Pretty (ppShow, valToStr)
import Utils (Position)

-- | 3-tuple of Position representing (start, wall, end)
type Cheat = (Position, Position)

race :: Maze -> Set.Set Position -> Cheat -> Int -> Maybe Int
race (Maze width height start end walls) borders (cheatStart, cheatEnd) maxDuration =
  let (toCheatStart, _) = dijkstra (Maze width height start cheatStart walls)
      (toCheatEnd, _) = dijkstra (Maze width height cheatStart cheatEnd borders) -- no walls!
      (toEnd, _) = dijkstra (Maze width height cheatEnd end walls)
   in if toCheatEnd <= maxDuration
        then
          Just (toCheatStart + toCheatEnd + toEnd)
        else
          Nothing

part1 :: (Maze, [Cheat]) -> Int
part1 (!maze, !cheats) =
  let (Maze width height start end walls) = maze
      borders = Set.fromList (getBorders maze)
      cheatless = fst (dijkstra maze)
      cheated = catMaybes (map (\c -> race maze borders c 2) cheats `using` parBuffer 6 rdeepseq)
      saved = parMap rdeepseq (cheatless -) cheated
   in length (filter (>= 100) saved)

part2 :: (Maze, [Cheat]) -> Int
part2 input = 0

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: ?
  print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?

processInput :: String -> (Maze, [Cheat])
processInput contents = (maze, cheats)
  where
    lns = lines contents
    height = length lns
    width = length (head lns)
    maze =
      foldr
        ( \(line, y) acc ->
            foldr
              ( \(ch, x) maze ->
                  let (Maze width height start end walls) = maze
                      position = (x, y)
                   in case ch of
                        '#' -> Maze width height start end (position `Set.insert` walls)
                        'S' -> Maze width height position end walls
                        'E' -> Maze width height start position walls
                        _ -> maze
              )
              acc
              (zip line [0 ..])
        )
        (Maze width height (-1, -1) (-1, -1) Set.empty)
        (zip lns [0 ..])

    cheats :: [Cheat]
    cheats = foldr (\p1 acc -> foldr (\p2 pairs -> (p1, p2) : pairs) acc nonWalls) [] nonWalls
      where
        nonWalls = getNonWalls maze
