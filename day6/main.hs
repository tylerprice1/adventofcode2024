import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Array
import Data.List qualified as List
import Data.Maybe (fromJust, fromMaybe)
import Data.Set qualified as Set
import Debug.Trace
import Text.Read (readMaybe)

type GridSize = (Int, Int) -- (Rows, Columns)

type Row = Int

type Column = Int

type Position = (Row, Column)

type Grid = Array Position Char

type Path = [DirectionalPosition]

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

getCurrentPosition :: Grid -> Maybe Position
getCurrentPosition grid =
  case List.find (\(_, ch) -> ch == '^') (assocs grid) of
    Nothing -> Nothing
    Just (position, ch) -> Just position

updateGridItem :: Grid -> Row -> Column -> (Char -> Char) -> Grid
updateGridItem grid row column f = grid // [((row, column), f (grid ! (row, column)))]

updateGrid :: Grid -> DirectionalPosition -> Grid
updateGrid grid (r, c, dir) = updateGridItem grid r c (const ((head . show) dir))

isPositionValid :: Grid -> DirectionalPosition -> Bool
isPositionValid grid (r, c, dir) = r >= minR && r <= maxR && c >= minC && c <= maxC
  where
    ((minR, minC), (maxR, maxC)) = bounds grid

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

navigate :: Grid -> DirectionalPosition -> Path -> Maybe (Path, Grid)
navigate grid position path
  | position `elem` path = Nothing -- already visited
  | otherwise =
      if isPositionValid grid advancedPosition
        then
          let (nextRow, nextColumn, _) = advancedPosition
              nextChar = grid ! (nextRow, nextColumn)
              newPosition
                | nextChar == 'O' || nextChar == '#' = turnedPosition
                | otherwise = advancedPosition
           in navigate grid newPosition updatedPath
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
        path = case navigate grid (row, column, U) [] of
          Nothing -> []
          Just (path, _) -> path
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
      (_, (height, width)) = bounds grid
      rows = [1 .. height]
      columns = [1 .. width]

      grids :: [Grid]
      grids =
        (unique . concat) [[updateGridItem grid r c (\c -> if c == '.' then 'O' else c) | c <- columns] | r <- rows]

      results = (map (\g -> navigate g (row, column, U) []) grids) `using` parListChunk 100 rdeepseq

processFile contents =
  let enumerate = List.zip [1 ..]
      ls = lines contents
      height = length ls
      width = (length . head) ls
   in array ((1, 1), (height, width)) (concat [[((r, c), ch) | (c, ch) <- enumerate l] | (r, l) <- enumerate ls])

main = do
  testFile <- readFile "./test.txt"
  let test = processFile testFile

  inputFile <- readFile "./input.txt"
  let input = processFile inputFile

  putStrLn "----- Part 1 -----"
  (print . fromJust) (part1 test) -- expected: 41
  (print . fromJust) (part1 input) -- expected: 4988
  --
  putStrLn "----- Part 2 -----"
  (print . fromJust) (part2 test) -- expected: 6
  (print . fromJust) (part2 input) -- expected: ?
  return ()
