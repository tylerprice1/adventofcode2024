import Data.Set qualified as Set
import Maze
import Path
import Position

part1 :: Maze -> Maybe Path
part1 = explore

part2 :: Maze -> Maze
part2 input = input

processInput :: String -> Maze
processInput contents = maze `setPosition` Position startX startY (Just East)
  where
    lns = lines contents
    height = length lns
    width = length (head lns)
    defaultPosition = Position (X (-1)) (Y (-1)) Nothing
    maze =
      foldr
        ( \(line, y) m ->
            foldr
              ( \(ch, x) m' ->
                  let (Maze w h position start end walls) = m'
                      p = Position (X x) (Y y) Nothing
                   in case ch of
                        '#' -> Maze w h position start end (p `Set.insert` walls)
                        'S' -> Maze w h position p end walls
                        'E' -> Maze w h position start p walls
                        _ -> m'
              )
              m
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

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: 7036
  print (part1 test2) -- Expected: 11048
  print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
