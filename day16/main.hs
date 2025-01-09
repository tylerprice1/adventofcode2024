import Data.List (sort)
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Base (maxInt)
import Text.Read (readMaybe)

newtype X = X Int
  deriving (Eq, Ord)

instance Show X where
  show (X x) = show x

instance Num X where
  (+) (X a) (X b) = X (a + b)
  (-) (X a) (X b) = X (a - b)
  (*) (X a) (X b) = X (a * b)
  negate (X a) = X (-a)
  abs (X a) = X (abs a)
  signum (X a) = X (signum a)
  fromInteger n = X (fromInteger n)

newtype Width = Width X
  deriving (Eq, Ord, Show)

newtype Y = Y Int
  deriving (Eq, Ord)

instance Show Y where
  show (Y y) = show y

instance Num Y where
  (+) (Y a) (Y b) = Y (a + b)
  (-) (Y a) (Y b) = Y (a - b)
  (*) (Y a) (Y b) = Y (a * b)
  negate (Y a) = Y (-a)
  abs (Y a) = Y (abs a)
  signum (Y a) = Y (signum a)
  fromInteger n = Y (fromInteger n)

newtype Height = Height Y
  deriving (Eq, Ord, Show)

data Orientation = North | South | East | West
  deriving (Eq, Ord)

instance Show Orientation where
  show North = "^"
  show South = "v"
  show East = ">"
  show West = "<"

data Position
  = Position {x :: X, y :: Y, orientation :: Maybe Orientation}

instance Ord Position where
  compare (Position x1 y1 o1) (Position x2 y2 o2) = case compare x1 x2 of
    EQ -> case compare y1 y2 of
      EQ -> case o1 of
        Nothing -> EQ
        (Just o1) -> case o2 of
          Nothing -> EQ
          (Just o2) -> compare o1 o2
      other -> other
    other -> other

instance Show Position where
  show (Position (X x) (Y y) Nothing) = "(" ++ show x ++ ", " ++ show y ++ ")"
  show (Position (X x) (Y y) (Just o)) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show o ++ ")"

updateX x (Position _ y o) = Position x y o

updateY y (Position x _ o) = Position x y o

updateOrientation o (Position x y _) = Position x y o

instance Eq Position where
  (==) (Position (X x1) (Y y1) Nothing) (Position (X x2) (Y y2) Nothing) = x1 == x2 && y1 == y2
  (==) (Position (X x1) (Y y1) Nothing) (Position (X x2) (Y y2) (Just _)) = x1 == x2 && y1 == y2
  (==) (Position (X x1) (Y y1) (Just _)) (Position (X x2) (Y y2) Nothing) = x1 == x2 && y1 == y2
  (==) (Position (X x1) (Y y1) (Just o1)) (Position (X x2) (Y y2) (Just o2)) = x1 == x2 && y1 == y2 && o1 == o2

data Maze = Maze
  { width :: Width,
    height :: Height,
    position :: Position,
    start :: Position,
    end :: Position,
    walls :: Set.Set Position
  }

updatePosition position (Maze width height _ start end walls) = Maze width height position start end walls

north (Position x (Y y) _) = Position x (Y (y - 1)) (Just North)

south (Position x (Y y) _) = Position x (Y (y + 1)) (Just South)

west (Position (X x) y _) = Position (X (x - 1)) y (Just West)

east (Position (X x) y _) = Position (X (x + 1)) y (Just East)

forward p = case fromJust (orientation p) of
  North -> north p
  East -> east p
  South -> south p
  West -> west p

clockwise p = case orientation p of
  Nothing -> error "No orientation"
  Just North -> Just East `updateOrientation` p
  Just East -> Just South `updateOrientation` p
  Just South -> Just West `updateOrientation` p
  Just West -> Just North `updateOrientation` p

counterclockwise p = case orientation p of
  Nothing -> error "No orientation"
  Just North -> Just West `updateOrientation` p
  Just West -> Just South `updateOrientation` p
  Just South -> Just East `updateOrientation` p
  Just East -> Just North `updateOrientation` p

instance Show Maze where
  show (Maze (Width (X width)) (Height (Y height)) position start end walls) =
    foldr
      ( \y s ->
          foldr
            ( \x s ->
                ( let p = Position (X x) (Y y) Nothing
                      ch
                        | p == position = maybe 'O' (head . show) (orientation position)
                        | p == start = 'S'
                        | p == end = 'E'
                        | p `Set.member` walls = '#'
                        | otherwise = '.'
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

explore :: Maze -> [Position] -> Int -> Int -> Int -> (Int, [[Position]])
explore maze path score minScore depth =
  let p = position maze
      (Position (X x) (Y y) orientation') = p -- trace (replicate depth ' ' ++ show p) p -- (trace (show p ++ "\n" ++ show (walls maze) ++ "\n" ++ show maze) p)
      orientation = fromJust orientation'
      path' = p : path
      turnedScore = 1000 + score
      forwardScore = 1 + score
      depth' = depth + 1
      (minScore', forwardResult) = explore (forward p `updatePosition` maze) path' forwardScore minScore depth'
      (minScore'', counterclockwiseResult) = explore (counterclockwise p `updatePosition` maze) path' turnedScore minScore' depth'
      (minScore''', clockwiseResult) = explore (clockwise p `updatePosition` maze) path' turnedScore minScore'' depth'
   in if x <= 0 || y <= 0
        then error "Invalid position"
        else
          if p `Set.member` walls maze || p `elem` path || score >= minScore
            then (minScore, [])
            else
              if p == end maze
                then (min minScore score, [path'])
                else
                  (minScore''', forwardResult ++ counterclockwiseResult ++ clockwiseResult)

part1 input =
  let (Position (X sx) (Y sy) _) = start input
      (Position (X ex) (Y ey) _) = end input
      (score, result) = explore input [] 0 (1000 * (abs (ey - sy) + abs (ex - sx))) 0
      lengths = map length result
   in score

part2 input = input

processInput :: String -> Maze
processInput contents = Position startX startY (Just East) `updatePosition` maze
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
    (Position startX startY _) = start maze

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: 7036
  print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
