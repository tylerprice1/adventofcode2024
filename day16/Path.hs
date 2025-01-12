module Path where

import Control.Applicative ((<|>))
import Control.Parallel (par, pseq)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
-- import Debug.Trace (trace, traceShow, traceShowId)
import Maze
import Position

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

data Path = Path
  { getMaze :: Maze,
    getActions :: [Action],
    getPositionPathMap :: Map.Map Position (Maybe Path),
    getVisited :: Set.Set Position
  }

setActions :: Path -> [Action] -> Path
setActions (Path m _ p v) actions = Path m actions p v

setMaze :: Path -> Maze -> Path
setMaze (Path _ a p v) maze = Path maze a p v

setPositionPathMap :: Path -> Map.Map Position (Maybe Path) -> Path
setPositionPathMap (Path m a _ v) p = Path m a p v

fromMaze :: Maze -> Path
fromMaze maze = Path maze [] Map.empty Set.empty

score :: Path -> Score
score path = foldr (\a s -> actionScore a + s) 0 (getActions path)
  where
    actionScore :: Action -> Score
    actionScore Forward = 1
    actionScore _ = 1_000

minPath :: Path -> Path -> Path
minPath a b = minimumBy (compare `on` score) [a, b]

min3Path :: Path -> Path -> Path -> Path
min3Path a b c = minimumBy (compare `on` score) [a, b, c]

explore :: Maze -> Maybe Path
explore maze = explore' (fromMaze maze) 0
  where
    exploreDirection :: Path -> Direction -> Int -> Maybe Path
    exploreDirection path direction depth =
      let (position, actions) = getPosition (getMaze path) `move` direction
          newPath = explore' (path `setMaze` (position `setPosition` maze)) (depth + 1)
       in (\p -> Just (p `setActions` (actions ++ getActions p))) =<< newPath

    explore' :: Path -> Int -> Maybe Path
    explore' path depth = Map.findWithDefault result position positionPathMap
      where
        depthStr = replicate depth ' '
        depth' = depth + 1
        (Path maze actions positionPathMap visited) = path
        (Maze _ _ position _ end walls) = maze

        result
          | depth > 100 = error ("Too deep\n" ++ show path)
          | position == end = Just (Path maze actions (Map.insert position result positionPathMap) (position `Set.insert` visited))
          | position `Set.member` walls = Nothing
          | position `Set.member` visited = Nothing
          | otherwise = case (getOrientation position) of
              Just North -> shortestPath North East West
              Just South -> shortestPath South East West
              Just East -> shortestPath East North South
              Just West -> shortestPath West North South
              Nothing -> error "No orientation"

        shortestPath :: Direction -> Direction -> Direction -> Maybe Path
        shortestPath a b c = case exploreDirection path a depth' of
          Nothing -> case exploreDirection path b depth' of
            -- !a, !b
            Nothing -> exploreDirection path c depth'
            Just bPath -> case exploreDirection (path `setPositionPathMap` getPositionPathMap bPath) c depth' of
              -- !a, b, !c
              Nothing -> Just bPath
              -- !a, b, c
              Just cPath -> Just (minPath bPath cPath `setPositionPathMap` getPositionPathMap cPath)
          --
          Just aPath -> case exploreDirection (path `setPositionPathMap` getPositionPathMap aPath) b depth' of
            Nothing -> case exploreDirection (path `setPositionPathMap` getPositionPathMap aPath) c depth' of
              -- a, !b, !c
              Nothing -> Just aPath
              -- a, !b, c
              Just cPath -> Just (minPath aPath cPath `setPositionPathMap` getPositionPathMap cPath)
            --
            Just bPath -> case exploreDirection (path `setPositionPathMap` getPositionPathMap bPath) c depth' of
              -- a, b, !c
              Nothing -> Just (minPath aPath bPath `setPositionPathMap` getPositionPathMap bPath)
              -- a, b, c
              Just cPath -> Just (min3Path aPath bPath cPath `setPositionPathMap` getPositionPathMap cPath)

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
                          | p == position = 'O'
                          | p `Map.member` visitedMap = maybe 'O' (maybe 'O' (head . show) . getOrientation) (p `Map.lookup` visitedMap)
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
      (Path maze actions positionPathMap visited) = path
      Maze (Width (X width)) (Height (Y height)) position start end walls = maze
      visitedMap = Map.fromList (map (\p -> (Position (getX p) (getY p) Nothing, p)) (Set.toList visited))
