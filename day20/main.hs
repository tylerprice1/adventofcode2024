module Main (main) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (dijkstra)
import GHC.List (foldl', foldr')
import Maze (Maze (..), getBorders, getNonWalls)
import Utils (Position)

-- | 3-tuple of Position representing (start, wall, end)
type Cheat = (Position, Position)

type Cache = Map.Map (Position, Position) Int

{-# INLINE findOrInsert #-}
findOrInsert :: Int -> (Position, Position) -> Cache -> (Int, Cache)
findOrInsert distance positions cache = case Map.lookup positions cache of
  Nothing -> (distance, Map.insert positions distance cache)
  Just distance' -> (distance', cache)

{-# INLINE race #-}
race :: Maze -> Set.Set Position -> Cheat -> Int -> Cache -> (Maybe Int, Cache)
race (Maze width height start end walls) borders (cheatStart, cheatEnd) maxDuration cache =
  let (toCheatEnd, _) = dijkstra (Maze width height cheatStart cheatEnd borders) -- no walls
   in if toCheatEnd <= maxDuration
        then
          let (toCheatStart, cache') = findOrInsert (fst (dijkstra (Maze width height start cheatStart walls))) (start, cheatStart) cache
              (toEnd, cache'') = findOrInsert (fst (dijkstra (Maze width height cheatEnd end walls))) (cheatEnd, end) cache'
           in (Just (toCheatStart + toCheatEnd + toEnd), cache'')
        else
          (Nothing, cache)

part1 :: (Maze, [Cheat]) -> Int
part1 (maze, cheats) =
  let borders = Set.fromList (getBorders maze)

      -- cheated = catMaybes (map (\c -> race maze borders c 2) cheats `using` parBuffer 6 rdeepseq)
      cheated :: [Int]
      cheated =
        fst
          ( foldl'
              ( \(cheated, cache) cheat ->
                  let (maybeDistance, cache') = race maze borders cheat 2 cache
                   in maybe (cheated, cache') (\distance -> (distance : cheated, cache')) maybeDistance
              )
              ([], Map.empty)
              cheats
          )

      cheatless = fst (dijkstra maze)
      saved = map (cheatless -) cheated
   in length (filter (>= 100) saved)

part2 :: (Maze, [Cheat]) -> Int
part2 input = 0

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let !test = processInput testFile

  inputFile <- readFile "./input.txt"
  let !input = processInput inputFile

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
