module Path (explore) where

import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as Set
import Direction (Direction (..))
import GHC.Base (maxInt)
import Maze (Maze (..))
import Position (Action (..), Height (..), Position (..), Width (..), X (..), Y (..), go, move, setOrientation)
import Score (Score (..))

data Path = Path Maze Score Position [Position]

instance Ord Path where
  compare (Path mazeA scoreA positionA positionsA) (Path mazeB scoreB positionB positionsB) =
    case compare scoreA scoreB of
      EQ -> case compare positionA positionB of
        EQ -> EQ
        other -> other
      other -> other

instance Eq Path where
  (==) a b = compare a b == EQ

type Cache = Map.Map Position Path

type Visited = Set.Set (Position, Score, Int)

consAction action (Path m s position positions) = Path m (s + actionScore action) (position `go` action) (position : positions)
  where
    actionScore :: Action -> Score
    actionScore Forward = 1
    actionScore _ = 1_000

getPosition :: Path -> Position
getPosition (Path _ _ p _) = p

fromMaze :: Maze -> Path
fromMaze maze = Path maze 0 (getStart maze) []

getScore :: Path -> Score
getScore (Path _ s _ _) = s

minPath :: Path -> Path -> Path
minPath a b = if getScore a <= getScore b then a else b

minMaybePath :: Maybe Path -> Maybe Path -> Maybe Path
minMaybePath Nothing Nothing = Nothing
minMaybePath Nothing (Just b) = Just b
minMaybePath (Just a) Nothing = Just a
minMaybePath (Just a) (Just b) = Just (minPath a b)

minimumMaybePath :: [Maybe Path] -> Maybe Path
minimumMaybePath maybePaths =
  case catMaybes maybePaths of
    [] -> Nothing
    paths -> Just (minimumBy (compare `on` getScore) paths)

showListPretty :: (Show a) => [a] -> String
showListPretty = foldr (\x s -> show x ++ "\n" ++ s) ""

explore :: Maze -> Maybe Path
explore maze =
  let (path, _, _, _) = explore' (fromMaze maze) Map.empty Set.empty (Score maxInt) 0
   in path

explore' :: Path -> Cache -> Visited -> Score -> Int -> (Maybe Path, Cache, Visited, Score)
explore' path cache visited minScore depth
  | depth > width * height = error ("Too deep " ++ "(" ++ show depth ++ ")" ++ "\n" ++ show path)
  | score >= minScore = (Nothing, cache, visited, minScore)
  | position == end = (Just path, Map.insert end path cache, visited, min minScore score)
  | position `Set.member` walls = (Nothing, cache, visited, minScore)
  | (position, score, depth) `Set.member` visited = (Nothing, cache, visited, minScore)
  | (position `setOrientation` Nothing) `elem` history = (Nothing, cache, visited, minScore)
  | otherwise =
      let path' = maybe path (minPath path) (Map.lookup position cache)
       in foldr
            ( \path (maybeShortest, cache, visited, minScore) ->
                let (path', cache', visited', minScore') = explore' path cache visited minScore depth'
                    maybeShortest' = minMaybePath path' maybeShortest
                 in (maybeShortest', cache', visited', minScore')
            )
            (Nothing, cache, visited', minScore)
            [ Forward `consAction` path',
              Forward `consAction` (Clockwise `consAction` path'),
              Forward `consAction` (Counterclockwise `consAction` path')
            ]
  where
    visited' = (position, score, depth) `Set.insert` visited
    (Path maze score position history) = path
    (Maze (Width (X width)) (Height (Y height)) _ end walls) = maze
    depthStr = replicate depth ' '
    depth' = depth + 1

instance Show Path where
  show path =
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
                          | p `Set.member` walls = '#'
                          | otherwise =
                              maybe
                                '.'
                                (head . show . fromJust . getOrientation)
                                (Map.lookup p visitedMap)
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
      -- ++ "\n"
      -- ++ foldr (\a s -> show a ++ "\n" ++ s) "" actions

      (Path maze score _ positions) = path
      Maze (Width (X width)) (Height (Y height)) start end walls = maze
      visited = positions
      visitedMap = Map.fromList (map (\(Position x y o) -> (Position x y Nothing, Position x y o)) visited)
