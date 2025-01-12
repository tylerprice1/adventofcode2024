module Path where

import Control.Applicative ((<|>))
import Control.Parallel (par, pseq)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
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

turnTo :: Position -> Direction -> (Position, [Action])
turnTo p direction = case getOrientation p of
  Just o
    | o == direction -> (p, [])
    | clockwise o == direction -> (p', [Clockwise])
    | counterclockwise o == direction -> (p', [Counterclockwise])
    | otherwise -> (p', [Clockwise, Clockwise])
  Nothing -> error "No orientation"
  where
    p' = p `setOrientation` Just direction

move :: Path -> Direction -> Path
move (Path maze actions positionPathMap visited) direction =
  let p = getPosition maze
      (turned, actions') = p `turnTo` direction
      p' = forward turned
   in Path (p' `setPosition` maze) (Forward : actions' ++ actions) positionPathMap (p `Set.insert` visited)

minPath :: Path -> Path -> Path
minPath a b = minimumBy (compare `on` score) [a, b]

min3Path :: Path -> Path -> Path -> Path
min3Path a b c = minimumBy (compare `on` score) [a, b, c]

explore :: Maze -> Maybe Path
explore maze = explore' (fromMaze maze) 0
  where
    explore' :: Path -> Int -> Maybe Path
    explore' path depth = Map.findWithDefault result position positionPathMap
      where
        depthStr = replicate depth ' '
        depth' = depth + 1
        (Path maze actions positionPathMap visited) = path
        position = getPosition maze

        result
          | position == getEnd maze =
              traceShowId
                ( Just
                    ( Path maze actions (Map.insert position result positionPathMap) (position `Set.insert` visited)
                    )
                )
          | position `Set.member` getWalls maze = Nothing
          | position `Set.member` getVisited path = Nothing
          | otherwise = case trace (depthStr ++ show position) (getOrientation position) of
              Just North -> shortestPath North East West
              Just South -> shortestPath South East West
              Just East -> shortestPath East North South
              Just West -> shortestPath West North South
              Nothing -> error "No orientation"

        shortestPath :: Direction -> Direction -> Direction -> Maybe Path
        shortestPath a b c = case explore' (path `move` a) depth' of
          Nothing -> case explore' (path `move` b) depth' of
            -- !a, !b
            Nothing -> explore' (path `move` c) depth'
            Just bPath -> case explore' ((path `setPositionPathMap` getPositionPathMap bPath) `move` c) depth' of
              -- !a, b, !c
              Nothing -> Just bPath
              -- !a, b, c
              Just cPath -> Just (minPath bPath cPath `setPositionPathMap` getPositionPathMap cPath)
          --
          Just aPath -> case explore' ((path `setPositionPathMap` getPositionPathMap aPath) `move` b) depth' of
            Nothing -> case explore' ((path `setPositionPathMap` getPositionPathMap aPath) `move` c) depth' of
              -- a, !b, !c
              Nothing -> Just aPath
              -- a, !b, c
              Just cPath -> Just (minPath aPath cPath `setPositionPathMap` getPositionPathMap cPath)
            --
            Just bPath -> case explore' ((path `setPositionPathMap` getPositionPathMap bPath) `move` c) depth' of
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
