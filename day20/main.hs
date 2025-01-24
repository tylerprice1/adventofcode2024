module Main (main) where

import Control.Parallel.Strategies (parBuffer, parList, parListChunk, parMap, rdeepseq, rpar, using)
-- import Debug.Trace (trace, traceShow, traceShowId)

import Data.List (group, sort)
import Data.List.Split (chunksOf)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (dijkstra)
import Maze (Maze (..), getNonWalls, showMazeWithPath)
import Text.Show.Pretty (ppShow, valToStr)
import Utils (Position)

-- | 3-tuple of Position representing (start, wall, end)
type Cheat = (Int, [Position])

part1 :: (Maze, [Cheat]) -> Int
part1 (maze, cheats) =
  let (Maze width height start end walls) = maze
      cheatless = fst (dijkstra maze)
      cheated =
        map
          ( \(_, cheatWalls) ->
              let cheatMaze = Maze width height start end (walls `Set.difference` Set.fromList cheatWalls)
                  (distance, path) = dijkstra cheatMaze
                  pathSet = Set.fromList path
               in if (2 == cheatless - distance && cheatless - distance <= 4)
                    then
                      trace
                        ( show distance
                            ++ " "
                            ++ show cheatWalls
                            ++ " "
                            ++ show path
                            ++ "\n"
                            ++ show cheatMaze
                            -- ++ "\n"
                            -- ++ showMazeWithPath cheatMaze path
                        )
                        (if any (`Set.notMember` pathSet) cheatWalls then cheatless else distance)
                    else
                      (if any (`Set.notMember` pathSet) cheatWalls then cheatless else distance)
          )
          cheats
          `using` parBuffer 6 rdeepseq
      saved = map (cheatless -) cheated
   in traceShow (map (\s -> (length s, head s)) (group (reverse (sort (filter (> 0) saved))))) (length (filter (>= 50) saved))

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
  -- print (part1 input) -- Expected: ?
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

    (Maze _ _ _ _ walls) = maze

    cheats :: [Cheat]
    cheats = filter ((<= 2) . fst) (map dijkstra cheatMazes `using` parBuffer 6 rdeepseq)
      where
        cheatMazes = map (\(p1, p2) -> Maze width height p1 p2 borders) (trace (ppShow pairs) pairs)
          where
            xs = [0 .. width - 1]
            ys = [0 .. height - 1]
            nonWalls = getNonWalls maze

            borders =
              Set.fromList
                ( foldr (\x borders -> (x, 0) : (x, height - 1) : borders) [] xs
                    ++ foldr (\y borders -> (0, y) : (width - 1, y) : borders) [] ys
                )

            pairs =
              fst
                ( foldr
                    ( \p1 acc ->
                        foldr
                          ( \p2 (pairs, added) ->
                              if (p1, p2) `Set.notMember` added && (p2, p1) `Set.notMember` added && p1 /= p2
                                then ((p1, p2) : pairs, (p1, p2) `Set.insert` ((p2, p1) `Set.insert` added))
                                else (pairs, added)
                          )
                          (acc)
                          nonWalls
                    )
                    ([], Set.empty)
                    nonWalls
                )
