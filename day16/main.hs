import Control.Applicative ((<|>))
import Control.DeepSeq (deepseq)
import Control.Exception (assert)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (NFData, parTraversable, rdeepseq, rpar, using)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Debug.Trace (trace)
import Dijkstra (dijkstra)
import Direction (Direction (..))
import GHC.Base (maxInt)
import GHC.Generics (Generic)
import Maze (Maze (..))
import Path (explore)
import Position (Height (..), Position (..), Width (..), X (..), Y (..), setOrientation)

showMazeWithPath maze path =
  foldr
    ( \y s ->
        foldr
          ( \x s ->
              ( let p = Position (X x) (Y y) Nothing
                    ch
                      | p == start = 'S'
                      | p == end = 'E'
                      | p `Set.member` walls = '#'
                      | otherwise = case p `Map.lookup` pathMap of
                          Nothing -> '.'
                          Just p' -> maybe 'O' (head . show) (getOrientation p')
                 in ch
              )
                : s
          )
          ""
          [1 .. width]
          ++ "\n"
          ++ s
    )
    ""
    [1 .. height]
  where
    Maze (Width (X width)) (Height (Y height)) start end walls = maze
    pathMap = Map.fromList (map (\p -> (p `setOrientation` Nothing, p)) path)

part1 maze =
  let (score, paths) = dijkstra maze
   in score

part2 :: Maze -> Int
part2 maze =
  let (score, paths) = dijkstra maze
   in trace (showMazeWithPath maze (map (`setOrientation` Nothing) (concat paths))) ((Set.size . Set.fromList) (map (`setOrientation` Nothing) (concat paths)))

processInput :: String -> Maze
processInput contents = maze
  where
    lns = lines contents
    height = length lns
    width = length (head lns)
    defaultPosition = Position (X (-1)) (Y (-1)) Nothing
    maze =
      foldr
        ( \(line, y) maze ->
            foldr
              ( \(ch, x) maze ->
                  let (Maze width height start end walls) = maze
                      withOrientation = Position (X x) (Y y)
                   in case ch of
                        '#' -> Maze width height start end (withOrientation Nothing `Set.insert` walls)
                        'S' -> Maze width height (withOrientation (Just East)) end walls
                        'E' -> Maze width height start (withOrientation Nothing) walls
                        _ -> maze
              )
              maze
              (zip line [1 ..])
        )
        (Maze (Width (X width)) (Height (Y height)) defaultPosition defaultPosition Set.empty)
        (zip lns [1 ..])

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  test2File <- readFile "./test2.txt"
  let test2 = processInput test2File

  test3File <- readFile "./test3.txt"
  let test3 = processInput test3File

  test4File <- readFile "./test4.txt"
  let test4 = processInput test4File

  test5File <- readFile "./test5.txt"
  let test5 = processInput test5File

  test6File <- readFile "./test6.txt"
  let test6 = processInput test6File

  test7File <- readFile "./test7.txt"
  let test7 = processInput test7File

  test8File <- readFile "./test8.txt"
  let test8 = processInput test8File

  test9File <- readFile "./test9.txt"
  let test9 = processInput test9File

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: 7036
  print (part1 test2) -- Expected: 11048
  -- print (part1 test3) -- Expected: 1004
  -- print (part1 test4) -- Expected: 3010
  -- print (part1 test5) -- Expected: 4013
  -- print (part1 test6) -- Expected: 21148
  -- print (part1 test7) -- Expected: 5078
  -- print (part1 test8) -- Expected: 21110
  -- print (part1 test9) -- Expected: 41210
  print (part1 input) -- Expected: ? NOT: 124476,
  --
  putStrLn "\n----- Part 2 -----"
  print (part2 test) -- Expected: 45
  print (part2 test2) -- Expected: 64
  print (part2 input) -- Expected: ?
