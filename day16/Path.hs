module Path where

-- import Debug.Trace (trace)

import Control.Applicative ((<|>))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Maze
import Position

trace a b = b

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

data Path = Path
  { getMaze :: Maze,
    getActions :: [Action],
    getPositionPathMap :: Map.Map Position (Maybe Path),
    getVisited :: Set.Set Position
  }

addActions :: Path -> [Action] -> Path
addActions path actions = path `setActions` (actions ++ getActions path)

setActions :: Path -> [Action] -> Path
setActions (Path m _ p v) actions = Path m actions p v

setMaze :: Path -> Maze -> Path
setMaze (Path _ a p v) maze = Path maze a p v

setVisited :: Set.Set Position -> Path -> Path
setVisited visited (Path m a p _) = Path m a p visited

getMazePosition :: Path -> Position
getMazePosition (Path maze _ _ _) = getPosition maze

setMazePosition :: Path -> Position -> Path
setMazePosition (Path maze a p v) position = Path (maze `setPosition` position) a p v

setPositionPathMap :: Path -> Map.Map Position (Maybe Path) -> Path
setPositionPathMap (Path m a _ v) p = Path m a p v

insertIntoPositionPathMap :: Position -> Maybe Path -> Path -> Path
insertIntoPositionPathMap position result path = path `setPositionPathMap` Map.insert position result (getPositionPathMap path)

insertIntoVisited :: Position -> Path -> Path
insertIntoVisited position path = (position `Set.insert` getVisited path) `setVisited` path

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
explore maze_ = explore' (fromMaze maze_) 0
  where
    explore' :: Path -> Int -> Maybe Path
    explore' path depth = case position `Map.lookup` positionPathMap of
      Nothing -> trace (depthStr ++ show position ++ " NOT FOUND") result -- trace (depthStr ++ show position) (Map.findWithDefault (trace (depthStr ++ show position ++ "NOT FOUND") result) position positionPathMap)
      Just Nothing -> trace (depthStr ++ show position ++ " NO PATH TO END") Nothing
      Just p -> trace (depthStr ++ show position ++ " CACHED PATH!") p
      where
        depthStr = replicate depth ' '
        depth' = depth + 1
        (Path maze actions positionPathMap visited) = path
        (Maze _ _ position _ end walls) = maze

        result
          | position == end =
              trace
                (depthStr ++ show position ++ " END")
                ( Just
                    ( Path
                        maze
                        actions
                        (Map.insert (position `setOrientation` Nothing) result positionPathMap)
                        (position `setOrientation` Nothing `Set.insert` visited)
                    )
                )
          | position `Set.member` walls = Nothing
          | position `Set.member` visited = Nothing
          | otherwise = case getOrientation position of
              Just North -> shortestPath position North East West
              Just South -> shortestPath position South East West
              Just East -> shortestPath position East North South
              Just West -> shortestPath position West North South
              Nothing -> error "No orientation"

        exploreDirection :: Path -> Direction -> Int -> Maybe Path
        exploreDirection path direction depth =
          let (position, actions) = getPosition (getMaze path) `move` direction
              newPath = explore' (getMazePosition path `insertIntoVisited` (path `setMaze` (getMaze path `setPosition` position))) (depth + 1)
           in ( \p ->
                  let updatedActions = p `setActions` (actions ++ getActions p)
                      updatedVisited = position `insertIntoVisited` updatedActions
                      updatedPositionPathMap = updatedVisited `setPositionPathMap` Map.insert (getMazePosition updatedVisited) (Just updatedVisited) (getPositionPathMap updatedVisited)
                   in Just updatedPositionPathMap
              )
                =<< newPath

        shortestPath :: Position -> Direction -> Direction -> Direction -> Maybe Path
        shortestPath position a b c =
          let visitedPath = position `insertIntoVisited` path

              usingPositionPathMapOf a b = a `setPositionPathMap` getPositionPathMap b

              (aPosition, aActions) = position `move` a
              aPath = (visitedPath `setMazePosition` aPosition) `addActions` aActions
              aPathResult = explore' aPath depth'
              -- need to insert aPosition's result into path's positionPathMap

              (bPosition, bActions) = position `move` b
              bPath' = (visitedPath `setMazePosition` bPosition) `addActions` bActions
              bPath = maybe bPath' (bPath' `usingPositionPathMapOf`) aPathResult
              bPathResult = explore' (insertIntoPositionPathMap aPosition aPathResult bPath) depth'
              -- need to insert bPosition's result into path's positionPathMap

              (cPosition, cActions) = position `move` c
              cPath' = (visitedPath `setMazePosition` cPosition) `addActions` cActions
              cPath = maybe cPath' (cPath' `usingPositionPathMapOf`) (bPathResult <|> aPathResult)
              cPathResult = explore' (insertIntoPositionPathMap bPosition bPathResult cPath) depth'
              -- need to insert cPosition's result into path's positionPathMap

              paths = catMaybes [aPathResult, bPathResult, cPathResult]
           in -- maybePath = case explore' aPath depth' of
              --   Nothing -> case explore' bPath depth' of
              --     -- !a, !b
              --     Nothing -> explore' cPath depth'
              --     Just bPathResult -> case explore' (cPath `usingPositionPathMapOf` bPathResult) depth' of
              --       -- !a, b, !c
              --       Nothing -> Just bPathResult
              --       -- !a, b, c
              --       Just cPath' -> Just (minPath bPath cPath' `usingPositionPathMapOf` cPath')
              --   Just aPathResult -> case exploreDirection (path `usingPositionPathMapOf` aPathResult) b depth' of
              --     Nothing -> case exploreDirection (path `usingPositionPathMapOf` aPathResult) c depth' of
              --       -- a, !b, !c
              --       Nothing -> Just aPathResult
              --       -- a, !b, c
              --       Just cPath -> Just (minPath aPathResult cPath `usingPositionPathMapOf` cPath)
              --     --
              --     Just bPath -> case exploreDirection (path `usingPositionPathMapOf` bPath) c depth' of
              --       -- a, b, !c
              --       Nothing -> Just (minPath aPathResult bPath `usingPositionPathMapOf` bPath)
              --       -- a, b, c
              --       Just cPath -> Just (min3Path aPathResult bPath cPath `usingPositionPathMapOf` cPath)
              if null paths then Nothing else Just (minimumBy (compare `on` score) paths) -- (\p -> Just (p `setPositionPathMap` Map.insert position (Just p) (getPositionPathMap p))) =<< maybePath

instance Show Path where
  show path =
    show (score path)
      ++ "\n"
      ++ foldr
        ( \y s ->
            foldr
              ( \x s' ->
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
                    : s'
              )
              ""
              [1 .. width]
              ++ "\n"
              ++ s
        )
        ""
        [1 .. height]
    where
      (Path maze _ _ visited) = path
      Maze (Width (X width)) (Height (Y height)) position start end walls = maze
      visitedMap = Map.fromList (map (\p -> (Position (getX p) (getY p) Nothing, p)) (Set.toList visited))
