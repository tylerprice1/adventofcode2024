import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Debug.Trace
import Text.Read (readMaybe)

enumerate = List.zip [0 ..]

type Grid = [String]

type GridSize = (Int, Int) -- (Rows, Columns)

type Row = Int

type Column = Int

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

type Position = (Row, Column)

type Path = [DirectionalPosition]

type DirectionalPosition = (Row, Column, Direction)

unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

updateListItem :: [a] -> Int -> (a -> a) -> [a]
updateListItem list index f = [if index == i then f l else l | (i, l) <- enumerate list]

updateGridItem :: Grid -> Row -> Column -> (Char -> Char) -> Grid
updateGridItem grid row column f = updateListItem grid row (\r -> updateListItem r column f)

getCurrentPosition :: Grid -> Maybe Position
getCurrentPosition grid =
  let maybeRow = List.findIndex ('^' `elem`) grid
   in case maybeRow of
        Nothing -> Nothing
        Just row ->
          let maybeColumn = List.elemIndex '^' (grid !! row)
           in case maybeColumn of
                Nothing -> Nothing
                Just column -> Just (row, column)

getGridSize :: Grid -> GridSize
getGridSize grid = (length grid, (length . head) grid)

updateGrid :: Grid -> DirectionalPosition -> Grid
updateGrid grid (r, c, dir) = updateGridItem grid r c (const ((head . show) dir))

isPositionValid :: Grid -> GridSize -> DirectionalPosition -> Bool
isPositionValid grid (height, width) (r, c, dir) = r >= 0 && r < height && c >= 0 && c < width

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

printGrid :: Grid -> IO ()
printGrid grid = mapM_ print [concat [c : " " | c <- row] | row <- grid]

navigate :: Grid -> GridSize -> DirectionalPosition -> Path -> Maybe (Path, Grid)
navigate grid size position path
  | position `elem` path = Nothing -- already visited
  | otherwise =
      if isPositionValid grid size advancedPosition
        then
          let (nextRow, nextColumn, _) = advancedPosition
              nextChar = grid !! nextRow !! nextColumn
              newPosition
                | nextChar == 'O' || nextChar == '#' = turnedPosition
                | otherwise = advancedPosition
           in navigate grid size newPosition updatedPath
        else
          Just (updatedPath, grid)
  where
    turnedPosition = turn position
    advancedPosition = advance position
    updatedPath = position : path

part1 :: Grid -> Maybe Int
part1 grid =
  case getCurrentPosition grid of
    Nothing -> Nothing
    Just (row, column) -> Just positionsCount
      where
        (path, _) = fromMaybe ([], []) (navigate grid (getGridSize grid) (row, column, U) [])
        positions = map removeDirection path
        uniquePositions = unique positions
        positionsCount = length uniquePositions

part2 :: Grid -> Maybe Int
part2 grid = case getCurrentPosition grid of
  Nothing -> Nothing
  Just position ->
    Just (length (filter (== Nothing) results))
    where
      (row, column) = position
      gridSize = getGridSize grid
      (height, width) = gridSize
      rows = [1 .. height]
      columns = [1 .. width]

      grids :: [Grid]
      grids =
        (unique . concat) [[updateGridItem grid (r - 1) (c - 1) (\c -> if c == '.' then 'O' else c) | c <- columns] | r <- rows]

      results' = map (\g -> navigate g gridSize (row, column, U) []) (traceShow (length grids) grids)
      results = results' `using` parList rdeepseq

main = do
  testFile <- readFile "./test.txt"
  let test = lines testFile

  inputFile <- readFile "./input.txt"
  let input = lines inputFile

  putStrLn "----- Part 1 -----"
  print (part1 test) -- expected: 41
  print (part1 input) -- expected: 4988
  --
  putStrLn "----- Part 2 -----"
  print (part2 test) -- expected: 6
  print (part2 input) -- expected: ?
  return ()
