module Path (explore) where

import Control.Applicative ((<|>))
import Control.Parallel (par, pseq)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShowId)
import Direction (Direction (..))
import Maze (Maze (..))
import Position (Action (..), Height (..), Position (..), Width (..), X (..), Y (..), go, move, setOrientation)
import Score (Score)

data Path = Path Maze [Action]
  deriving (Eq)

instance Ord Path where
  compare (Path mazeA actionsA) (Path mazeB actionsB) = case compare actionsA actionsB of
    EQ -> compare mazeA mazeB
    other -> other

type Cache = Map.Map Position Path

type Visited = Set.Set Path

getPositions :: Path -> [Position]
getPositions (Path (Maze _ _ start _ _) actions) = foldr (\a (p : ps) -> (p `go` a) : p : ps) [start] actions

getPosition :: Path -> Position
getPosition (Path (Maze _ _ start _ _) actions) = foldr (flip go) start actions

fromMaze :: Maze -> Path
fromMaze maze = Path maze []

score :: Path -> Score
score (Path _ actions) = foldr (\a s -> actionScore a + s) 0 actions
  where
    actionScore :: Action -> Score
    actionScore Forward = 1
    actionScore _ = 1_000

minPath :: Path -> Path -> Path
minPath a b = minimumBy (compare `on` score) [a, b]

min3Path :: Path -> Path -> Path -> Path
min3Path a b c = minimumBy (compare `on` score) [a, b, c]

minimumMaybePath :: [Maybe Path] -> Maybe Path
minimumMaybePath maybePaths =
  case catMaybes maybePaths of
    [] -> Nothing
    paths -> Just (minimumBy (compare `on` score) paths)

minimumPath :: [Path] -> Path
minimumPath = minimumBy (compare `on` score)

showListPretty :: (Show a) => [a] -> String
showListPretty = foldr (\x s -> show x ++ "\n" ++ s) ""

explore :: Maze -> Maybe Path
explore maze =
  let (path, cache, visited) = explore' (fromMaze maze) Map.empty Set.empty 0
   in {- trace (show path ++ "\n" ++ showListPretty (Map.toList cache) ++ "\n" ++ showListPretty (Set.toList visited)) -} (path)

explore' :: Path -> Cache -> Visited -> Int -> (Maybe Path, Cache, Visited)
explore' path cache visited depth
  | depth > width * height = error ("Too deep " ++ "(" ++ show depth ++ ")" ++ "\n" ++ show path)
  | position == end = (Just path, Map.insert end path cache, visited')
  | path `Set.member` visited = (Nothing, cache, visited)
  | position `Set.member` walls = (Nothing, cache, visited')
  | (position `setOrientation` Nothing) `elem` history = (Nothing, cache, visited')
  | otherwise =
      let (Path maze actions) = case Map.lookup position cache of
            Nothing -> path
            Just path' -> minPath path path'
          (forwardPath, forwardCache, forwardVisited) = explore' (Path maze (Forward : actions)) cache visited' depth'
          (clockwisePath, clockwiseCache, clockwiseVisited) = explore' (Path maze (Forward : Clockwise : actions)) forwardCache forwardVisited depth'
          (counterclockwisePath, counterclockwiseCache, counterclockwiseVisited) = explore' (Path maze (Forward : Counterclockwise : actions)) clockwiseCache clockwiseVisited depth'
          maybePaths = [forwardPath, clockwisePath, counterclockwisePath]
          shortest = minimumMaybePath maybePaths
       in (shortest, Map.insert position path counterclockwiseCache, counterclockwiseVisited)
  where
    visited' = path `Set.insert` visited
    (Path maze _) = path
    (Maze (Width (X width)) (Height (Y height)) _ end walls) = maze
    (position : history) = getPositions path
    depthStr = replicate depth ' '
    depth' = depth + 1

instance Show Path where
  show path =
    show (score path)
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
      ++ "\n"
      ++ showListPretty visited
    where
      -- ++ "\n"
      -- ++ foldr (\a s -> show a ++ "\n" ++ s) "" actions

      (Path maze actions) = path
      Maze (Width (X width)) (Height (Y height)) start end walls = maze
      visited = getPositions path
      visitedMap = Map.fromList (map (\(Position x y o) -> (Position x y Nothing, Position x y o)) visited)
