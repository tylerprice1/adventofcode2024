module Path (explore) where

import Control.Applicative ((<|>))
import Control.Parallel (par, pseq)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as Set
import Debug.Trace (traceShowId)
import Direction (Direction (..))
import Maze (Maze (..))
import Position (Action (..), Height (..), Position (..), Width (..), X (..), Y (..), go, move)
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
getPositions (Path (Maze _ _ start _ _) actions) = reverse (foldr (\a (p : ps) -> (p `go` a) : p : ps) [start] actions)

getPosition :: Path -> Position
getPosition = head . getPositions

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

explore :: Maze -> Maybe Path
explore maze =
  let (p, _, _) = explore' (fromMaze maze) Map.empty Set.empty 0
   in p

explore' :: Path -> Cache -> Visited -> Int -> (Maybe Path, Cache, Visited)
explore' path cache visited depth = case Map.lookup position cache of
  Just path' ->
    let shorter = minPath path path'
     in (Just path, Map.insert position shorter cache, visited')
  Nothing
    | depth > width * height -> error ("Too deep " ++ "(" ++ show depth ++ ")" ++ "\n" ++ show path)
    | path `Set.member` visited -> (Nothing, cache, visited)
    | position `elem` positions -> (Nothing, cache, visited')
    | position == end -> (Just path, Map.insert end path cache, visited')
    | position `Set.member` walls -> (Nothing, cache, visited')
    | otherwise ->
        let (forwardPath, forwardCache, forwardVisited) = explore' (Path maze (Forward : actions)) cache visited' depth'
            (clockwisePath, clockwiseCache, clockwiseVisited) = explore' (Path maze (Clockwise : actions)) forwardCache forwardVisited depth'
            (counterclockwisePath, counterclockwiseCache, counterclockwiseVisited) = explore' (Path maze (Counterclockwise : actions)) clockwiseCache clockwiseVisited depth'
            maybePaths = [forwardPath, clockwisePath, counterclockwisePath]
            shortest = minimumMaybePath maybePaths
         in (shortest, counterclockwiseCache, counterclockwiseVisited)
  where
    visited' = path `Set.insert` visited
    (Path maze actions) = path
    (Maze (Width (X width)) (Height (Y height)) _ end walls) = maze
    (position : positions) = getPositions path
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
      ++ foldr (\a s -> show a ++ "\n" ++ s) "" actions
    where
      (Path maze actions) = path
      Maze (Width (X width)) (Height (Y height)) start end walls = maze
      visited = getPositions path
      visitedMap = Map.fromList (map (\(Position x y o) -> (Position x y Nothing, Position x y o)) visited)
