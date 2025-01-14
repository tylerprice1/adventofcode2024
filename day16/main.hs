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
import GHC.Base (maxInt)
import GHC.Generics (Generic)
import Journey (embark)
import Maze
import Path
import Position

part1 = embark

part2 :: Maze -> Maze
part2 input = input

processInput :: String -> Maze
processInput contents = Position startX startY (Just East) `setPosition` maze
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
                  let (Maze width height position start end walls) = maze
                      p = Position (X x) (Y y) Nothing
                   in case ch of
                        '#' -> Maze width height position start end (p `Set.insert` walls)
                        'S' -> Maze width height position p end walls
                        'E' -> Maze width height position start p walls
                        _ -> maze
              )
              maze
              (zip line [1 ..])
        )
        (Maze (Width (X width)) (Height (Y height)) defaultPosition defaultPosition defaultPosition Set.empty)
        (zip lns [1 ..])
    (Position startX startY _) = getStart maze

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
  -- print (part1 test2) -- Expected: 11048
  -- print (part1 test3) -- Expected: 1004
  -- print (part1 test4) -- Expected: 3010
  -- print (part1 test5) -- Expected: 4013
  -- print (part1 test6) -- Expected: 21148
  -- print (part1 test7) -- Expected: 5078
  -- print (part1 test8) -- Expected: 21110
  -- print (part1 test9) -- Expected: 41210
  -- print (part1 input) -- Expected: ? NOT: 124476,
  --
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
