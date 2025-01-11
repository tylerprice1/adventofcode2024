import Control.Parallel (par, pseq)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
-- import Debug.Trace (trace)
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

newtype Path = Path {getSet :: Set.Set Position}
  deriving (Show)

unionPath :: Path -> Path -> Path
unionPath a b = Path (getSet a `Set.union` getSet b)

data Result = Result {getScore :: Maybe Score, getPath :: Path}
  deriving (Show)

processResults :: Score -> Result -> Result -> Result -> Result
processResults minScore f cw ccw = f `processPair` cw `processPair` ccw
  where
    processPair :: Result -> Result -> Result
    processPair a b = case (a, b) of
      (Result Nothing aPath, Result Nothing bPath) -> Result Nothing (aPath `unionPath` bPath)
      (Result score aPath, Result Nothing bPath) -> Result score bPath
      (Result Nothing aPath, Result score bPath) -> Result score aPath
      (Result (Just aScore) aPath, Result (Just bScore) bPath)
        | aScore >= minScore && bScore >= minScore -> Result Nothing (aPath `unionPath` bPath)
        | aScore >= minScore -> Result (Just bScore) aPath
        | bScore >= minScore -> Result (Just aScore) bPath
        | aScore <= bScore -> Result (Just aScore) bPath
        | otherwise -> Result (Just bScore) aPath

explore :: Maze -> Path -> Score -> Score -> Int -> Result
explore maze path score minScore depth =
  let p = getPosition maze
      depthStr = replicate depth '\t'
      path' = Path (p `Set.insert` getSet path)
      result
        | p == getEnd maze = trace (depthStr ++ "END: " ++ show score) (Result (if score < minScore then Just score else Nothing) path')
        | score >= minScore = trace (depthStr ++ "WORSE: " ++ show p ++ " " ++ show score) (Result Nothing path')
        | p `Set.member` getWalls maze = trace (depthStr ++ "WALL: " ++ show p ++ " " ++ show score) (Result Nothing path')
        | p `Set.member` getSet path = trace (depthStr ++ "VISITED: " ++ show p ++ " " ++ show score) (Result Nothing path')
        | otherwise =
            let turnedScore = 1000 + score
                forwardScore = 1 + score
                depth' = depth + 1
                fPosition = forward p `setPosition` maze
                cwPosition = clockwise p `setPosition` maze
                ccwPosition = counterclockwise p `setPosition` maze

                fTrace = depthStr ++ "FRWD: " ++ show p ++ " -> " ++ show (forward p) ++ " " ++ show score ++ " -> " ++ show forwardScore
                cwTrace = depthStr ++ "CW:   " ++ show p ++ " -> " ++ show (clockwise p) ++ " " ++ show score ++ " -> " ++ show turnedScore
                ccwTrace = depthStr ++ "CCW:  " ++ show p ++ " -> " ++ show (counterclockwise p) ++ " " ++ show score ++ " -> " ++ show turnedScore

                fResult = explore fPosition path' forwardScore minScore depth'
                cwResult = explore cwPosition path' turnedScore minScore depth'
                ccwResult = explore ccwPosition path' turnedScore minScore depth'
             in -- fResult = case explore fPosition path' forwardScore minScore depth' of
                --   Left r -> r
                --   Right (f, cw, ccw) -> processResults minScore f cw ccw

                -- cwResult = case explore cwPosition path' turnedScore minScore depth' of
                --   Left r -> r
                --   Right (f, cw, ccw) -> processResults minScore f cw ccw

                -- ccwResult = case explore ccwPosition path' turnedScore minScore depth' of
                --   Left r -> r
                --   Right (f, cw, ccw) -> processResults minScore f cw ccw
                processResults minScore fResult cwResult ccwResult
   in result

part1 :: Maze -> Result
part1 input =
  let (Position (X sx) (Y sy) _) = getStart input
      (Position (X ex) (Y ey) _) = getEnd input
      result = explore input (Path Set.empty) 0 (Score (1000 * (abs (ey - sy) + abs (ex - sx)))) 0
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
