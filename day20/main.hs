import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as Set
import Dijkstra (dijkstra)
import Maze (Maze (..))
import Utils (Position, east, north, south, west)

-- | 3-tuple of Position representing (start, wall, end)
type Cheat = (Position, Position, Position)

part1 (maze, cheats) =
  let (Maze width height start end walls) = maze
      cheatless = fst (dijkstra maze)
      cheated = parMap rdeepseq (\(pre, wall, post) -> fst (dijkstra (Maze width height start end (wall `Set.delete` walls)))) cheats
      saved = sortBy (comparing Down) (map (cheatless -) cheated)
   in length (filter (>= 100) saved)

part2 input = input

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
    defaultPosition = (-1, -1)
    maze =
      foldr
        ( \(line, y) maze ->
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
              maze
              (zip line [0 ..])
        )
        (Maze width height defaultPosition defaultPosition Set.empty)
        (zip lns [0 ..])

    inMaze (x, y) = x >= 0 && x < width && y >= 0 && y < height

    walls = getWalls maze

    isCheat :: Cheat -> Bool
    isCheat (pre, wall, post) =
      inMaze pre
        && not (pre `Set.member` walls)
        && inMaze wall
        && wall `Set.member` walls
        && inMaze post
        && not (post `Set.member` walls)

    cheats :: [Cheat]
    cheats = sort (concatMap (\w -> filter isCheat [(south w, w, north w), (west w, w, east w)]) walls)
