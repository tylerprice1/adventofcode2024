import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace
import Text.Read (readMaybe)

type GridSize = (Int, Int) -- (Rows, Columns)

type Row = Int

type Column = Int

type Position = (Row, Column)

type Grid = Vector.Vector (Vector.Vector Char)

type Visited = Set.Set DirectionalPosition

type DirectionalPosition = (Row, Column, Direction)

data Direction = L | U | R | D
  deriving (Eq, Ord)

instance NFData Direction where
  rnf _ = ()

instance Show Direction where
  show direction = case direction of
    L -> "<"
    U -> "^"
    R -> ">"
    D -> "v"

instance Read Direction where
  readsPrec _ str = case str of
    ('<' : s) -> [(L, s)]
    ('^' : s) -> [(U, s)]
    ('>' : s) -> [(R, s)]
    ('v' : s) -> [(D, s)]
    _ -> []

unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

getCurrentPosition :: Grid -> Position
getCurrentPosition grid =
  let row = Maybe.fromJust (Vector.findIndex (\r -> '^' `elem` r) grid)
      column = Maybe.fromJust (Vector.findIndex (== '^') (grid Vector.! row))
   in (row, column)

updateGridItem :: Grid -> Row -> Column -> (Char -> Char) -> Grid
updateGridItem grid row column f = grid Vector.// [(row, (grid Vector.! row) Vector.// [(column, f (grid Vector.! row Vector.! column))])]

updateGrid :: Grid -> DirectionalPosition -> Grid
updateGrid grid (r, c, dir) = updateGridItem grid r c (const ((head . show) dir))

getGridSize grid = (Vector.length grid, (Vector.length . Vector.head) grid)

isPositionValid :: Grid -> DirectionalPosition -> Bool
isPositionValid grid (r, c, dir) = r >= minR && r < maxR && c >= minC && c < maxC
  where
    minR = 0
    minC = 0
    (maxR, maxC) = getGridSize grid

advance :: DirectionalPosition -> DirectionalPosition
advance (r, c, dir) = case dir of
  L -> (r, c - 1, dir)
  U -> (r - 1, c, dir)
  R -> (r, c + 1, dir)
  D -> (r + 1, c, dir)

turn :: DirectionalPosition -> DirectionalPosition
turn (row, column, direction) =
  let newDirection = case direction of
        L -> U
        U -> R
        R -> D
        D -> L
   in (row, column, newDirection)

addDirection :: Position -> Direction -> DirectionalPosition
addDirection (row, column) direction = (row, column, direction)

removeDirection :: DirectionalPosition -> Position
removeDirection (row, column, _) = (row, column)

navigate :: Grid -> DirectionalPosition -> Visited -> Maybe (Visited, Grid)
navigate grid position visited
  | position `Set.member` visited = Nothing -- already visited
  | otherwise =
      if isPositionValid grid advancedPosition
        then
          let (nextRow, nextColumn, _) = advancedPosition
              nextChar = grid Vector.! nextRow Vector.! nextColumn
              newPosition
                | nextChar == 'O' || nextChar == '#' = turnedPosition
                | otherwise = advancedPosition
           in navigate grid newPosition updatedVisited
        else
          Just (updatedVisited, grid)
  where
    turnedPosition = turn position
    advancedPosition = advance position
    updatedVisited = Set.insert position visited

part1 :: Grid -> Int
part1 grid = positionsCount
  where
    (row, column) = getCurrentPosition grid
    visited = case navigate grid (row, column, U) Set.empty of
      Nothing -> Set.empty
      Just (visited, _) -> visited
    positionsCount = Set.size (Set.map removeDirection visited)

part2 :: Grid -> Int
part2 grid = length (filter id results)
  where
    position = getCurrentPosition grid
    (row, column) = position
    (height, width) = getGridSize grid
    rows = [0 .. height - 1]
    columns = [0 .. width - 1]

    grids :: [Grid]
    grids =
      (unique . concat) [[updateGridItem grid r c (\c -> if c == '.' then 'O' else c) | c <- columns] | r <- rows]

    results = map (\g -> Maybe.isNothing (navigate g (row, column, U) Set.empty)) grids `using` parListChunk 100 rdeepseq

processFile :: String -> Grid
processFile contents = Vector.fromList [Vector.fromList l | l <- lines contents]

main = do
  testFile <- readFile "./test.txt"
  let test = processFile testFile

  inputFile <- readFile "./input.txt"
  let input = processFile inputFile

  putStrLn "----- Part 1 -----"
  print (part1 test) -- expected: 41
  print (part1 input) -- expected: 4988
  --
  putStrLn "----- Part 2 -----"
  print (part2 test) -- expected: 6
  print (part2 input) -- expected: 1697
  return ()
