module Main (main) where

import Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (Distance, dijkstra)
import Maze (Maze (..), getBorders, getNonWalls, showMazeWithPath)
import Utils (Position)

-- | 2-tuple of Position representing (start, end)
type Cheat = (Position, Position)

race :: Maze -> Set.Set Position -> Cheat -> Int -> Maybe Distance
{-# INLINE race #-}
race (Maze width height start end walls) borders (cheatStart, cheatEnd) maxCheatDuration =
  let (!toCheatEnd, _) = dijkstra (Maze width height cheatStart cheatEnd borders) -- no walls
   in if toCheatEnd <= maxCheatDuration
        then
          let (!toCheatStart, _) = dijkstra (Maze width height start cheatStart walls)
              (!toEnd, _) = dijkstra (Maze width height cheatEnd end walls)
           in Just (toCheatStart + toCheatEnd + toEnd)
        else
          Nothing

part1 :: (Maze, [Cheat]) -> Int
part1 (maze, cheats) =
  let (Maze width height start end walls) = trace "Maze" maze
      borders = trace "Borders" Set.fromList (getBorders maze)
      cheated = trace "Cheated" catMaybes (map (\c -> race maze borders c 2) cheats `using` parBuffer 7 rdeepseq)

      cheatless = trace "Cheatless" fst (dijkstra maze)
      saved = trace "Saved" map (cheatless -) cheated
   in length (filter (>= 100) saved)

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

processInput :: String -> (Maze, [Cheat])
processInput contents = (maze, cheats)
  where
    lns = lines contents
    height = fromIntegral (length lns)
    width = fromIntegral (length (head lns))
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
    cheats = foldl (\acc p1 -> foldl (\pairs p2 -> (p1, p2) : pairs) acc nonWalls) [] nonWalls
      where
        nonWalls = getNonWalls maze
