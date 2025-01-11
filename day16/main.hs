-- import Debug.Trace (trace)

import Control.Applicative ((<|>))
import Control.Parallel (par, pseq)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import GHC.Base (maxInt)

trace _ a = a

min3 :: (Ord a) => a -> a -> a -> a
{-# SPECIALIZE min3 :: Score -> Score -> Score -> Score #-}
min3 a b c = min a (min b c)

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

newtype X = X Int
  deriving (Eq, Ord, Num, Show)

newtype Width = Width X
  deriving (Eq, Ord, Show)

newtype Y = Y Int
  deriving (Eq, Ord, Num, Show)

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
  show (Position x y o) =
    "("
      ++ show x
      ++ ", "
      ++ show y
      ++ ( case o of
             Nothing -> ""
             Just o -> ", " ++ show o
         )
      ++ ")"

updateOrientation o (Position x y _) = Position x y o

instance Eq Position where
  (==) (Position x1 y1 o1) (Position x2 y2 o2) = x1 == x2 && y1 == y2 && (isNothing o1 || isNothing o2 || o1 == o2)

data Maze = Maze
  { getWidth :: Width,
    getHeight :: Height,
    getPosition :: Position,
    getStart :: Position,
    getEnd :: Position,
    getWalls :: Set.Set Position
  }

setPosition :: Position -> Maze -> Maze
setPosition position (Maze width height _ start end walls) = Maze width height position start end walls

north :: Position -> Position
north (Position x y _) = Position x (y - 1) (Just North)

south :: Position -> Position
south (Position x y _) = Position x (y + 1) (Just South)

west :: Position -> Position
west (Position x y _) = Position (x - 1) y (Just West)

east :: Position -> Position
east (Position x y _) = Position (x + 1) y (Just East)

forward :: Position -> Position
forward p = case fromJust (orientation p) of
  North -> north p
  East -> east p
  South -> south p
  West -> west p

clockwise :: Position -> Position
clockwise p = case orientation p of
  Nothing -> error "No orientation"
  Just North -> Just East `updateOrientation` p
  Just East -> Just South `updateOrientation` p
  Just South -> Just West `updateOrientation` p
  Just West -> Just North `updateOrientation` p

counterclockwise :: Position -> Position
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

data Action = Forward | Clockwise | Counterclockwise

data Path = Path
  { getMaze :: Maze,
    getScore :: Score,
    getVisited :: Set.Set Position
  }

instance Show Path where
  show (Path maze score visited) =
    show score
      ++ "\n"
      ++ foldr
        ( \y s ->
            foldr
              ( \x s ->
                  ( let p = Position (X x) (Y y) Nothing
                        ch
                          | p == start = 'S'
                          | p == end = 'E'
                          | p `Map.member` visitedMap = maybe 'O' (head . show) ((orientation . fromJust) (p `Map.lookup` visitedMap))
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
    where
      Maze (Width (X width)) (Height (Y height)) position start end walls = maze
      visitedMap = Map.fromList (map (\p -> (Position (x p) (y p) Nothing, p)) (Set.elems visited))

move (Path maze score visited) action = case action of
  Forward -> Path (forward position `setPosition` maze) (score + 1) newVisited
  Clockwise -> Path (clockwise position `setPosition` maze) (score + 1000) newVisited
  Counterclockwise -> Path (counterclockwise position `setPosition` maze) (score + 1000) newVisited
  where
    position = getPosition maze
    newVisited = position `Set.insert` visited

explore maze = explore' (Path maze 0 Set.empty)
  where
    explore' :: Path -> Maybe Path
    explore' path
      | position == getEnd maze = Just path
      | position `Set.member` getWalls maze = Nothing
      | position `Set.member` getVisited path = Nothing
      | otherwise = explore' (path `move` Forward) <|> explore' (path `move` Clockwise) <|> explore' (path `move` Counterclockwise)
      where
        maze = getMaze path
        position = getPosition maze

part1 input =
  let (Position (X sx) (Y sy) _) = getStart input
      (Position (X ex) (Y ey) _) = getEnd input
      result = explore input
   in result

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

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: 7036
  -- print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
