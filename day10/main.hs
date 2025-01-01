import Data.Maybe (catMaybes, isJust)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace (traceShow, traceShowId)

data GridNode = Position Height (Maybe GridNode) (Maybe GridNode) (Maybe GridNode) (Maybe GridNode)

processInput' :: String -> [[GridNode]]
processInput' contents =
  let grid = Vector.fromList [Vector.fromList [read [ch] :: Int | ch <- l] | l <- lines contents]
      unlinkedNodes :: Vector.Vector (Vector.Vector GridNode)
      unlinkedNodes =
        Vector.map
          (Vector.map (\height -> Position height Nothing Nothing Nothing Nothing))
          grid

      getNode :: Int -> Int -> Maybe GridNode
      getNode r c = case (Vector.!?) unlinkedNodes r of
        Nothing -> Nothing
        Just row -> (Vector.!?) row c

      getUp r = getNode (r - 1)
      getDown r = getNode (r + 1)
      getLeft r c = getNode r (c - 1)
      getRight r c = getNode r (c + 1)

      linkedNodes :: Vector.Vector (Vector.Vector GridNode)
      linkedNodes = Vector.imap (\r row -> Vector.imap (\c height -> Position height (getLeft r c) (getUp r c) (getRight r c) (getDown r c)) row) grid
   in Vector.toList (Vector.map Vector.toList linkedNodes)

type Height = Int

type RowIndex = Int

type ColumnIndex = Int

type Position = (RowIndex, ColumnIndex)

type Row = Vector.Vector Height

type TopologicalMap = Vector.Vector Row

type Trail = Vector.Vector Position

type TrailHead = Position

data Direction = U | D | L | R
  deriving (Eq, Show)

hasPosition :: TopologicalMap -> Position -> Bool
hasPosition grid (r, c) =
  let rowCount = Vector.length grid
      columnCount = Vector.length ((Vector.!) grid r)
   in r >= 0 && r < rowCount && c >= 0 && c < columnCount

getIfHasPosition :: TopologicalMap -> Position -> Maybe Position
getIfHasPosition grid pos = if hasPosition grid pos then Just pos else Nothing

getHeightAtPosition :: TopologicalMap -> Position -> Height
getHeightAtPosition grid (r, c) = (Vector.!) ((Vector.!) grid r) c

getPositionInDirection :: TopologicalMap -> Position -> Direction -> Maybe Position
getPositionInDirection grid (r, c) direction =
  getIfHasPosition
    grid
    ( case direction of
        L -> (r, c - 1)
        U -> (r - 1, c)
        R -> (r, c + 1)
        D -> (r + 1, c)
    )

getSurroundingPositions :: TopologicalMap -> Position -> Vector.Vector Position
getSurroundingPositions map (row, column) =
  let getPosition = getPositionInDirection map (row, column)
   in Vector.fromList (catMaybes [getPosition L, getPosition U, getPosition R, getPosition D])

filterPositions :: (Height -> Bool) -> TopologicalMap -> Vector.Vector Position
filterPositions predicate grid = Vector.concatMap (findInRow predicate) (Vector.indexed grid)
  where
    findInRow :: (Height -> Bool) -> (RowIndex, Row) -> Vector.Vector Position
    findInRow predicate (rowIndex, row) =
      Vector.imapMaybe
        ( \column height ->
            if predicate height
              then Just (rowIndex, column)
              else Nothing
        )
        row

findTrailHeads = filterPositions (== 0)

exploreTrail :: TopologicalMap -> TrailHead -> Vector.Vector Trail
exploreTrail grid trailHead = explore grid trailHead []
  where
    explore :: TopologicalMap -> Position -> [Position] -> Vector.Vector Trail
    explore grid position reversedPath =
      let height = getHeightAtPosition grid position
          updatedPath = position : reversedPath
          nextPositions = Vector.filter (\pos -> getHeightAtPosition grid pos == height + 1) (getSurroundingPositions grid position)
       in if position `elem` reversedPath
            then Vector.empty
            else
              if height == 9
                then Vector.singleton (Vector.fromList (reverse updatedPath))
                else Vector.concatMap (\pos -> explore grid pos updatedPath) nextPositions

getTrailScore :: TopologicalMap -> TrailHead -> Int
getTrailScore grid trailHead =
  let allTrails = exploreTrail grid trailHead
   in Set.size (Set.fromList (Vector.toList (Vector.map (\trail -> (Vector.head trail, Vector.last trail)) allTrails)))

part1 input =
  let trailHeads = findTrailHeads input
   in Vector.sum (Vector.map (getTrailScore input) trailHeads)

part2 input =
  let trailHeads = findTrailHeads input
   in Vector.length (Vector.concatMap (exploreTrail input) trailHeads)

processInput :: String -> TopologicalMap
processInput contents = Vector.fromList [Vector.fromList [read [ch] :: Int | ch <- l] | l <- lines contents]

showTrail :: TopologicalMap -> Trail -> Vector.Vector String
showTrail grid trail = Vector.imap showColumns grid
  where
    showColumns rowIndex row =
      concat
        ( Vector.imap
            ( \columnIndex height ->
                if Vector.elem (rowIndex, columnIndex) trail
                  then show height
                  else "."
            )
            row
        )

printGrid :: (Show a) => Vector.Vector a -> IO ()
printGrid = Vector.mapM_ print

printTrail grid trail = printGrid (showTrail grid trail)

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "----- Part 1 -----"
  print (part1 test) -- Expected: 36
  print (part1 input) -- Expected: 548
  putStrLn "----- Part 2 -----"
  print (part2 test) -- Expected: 81
  print (part2 input) -- Expected: ?
