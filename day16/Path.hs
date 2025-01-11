module Path where

import Control.Applicative ((<|>))
import Control.Parallel (par, pseq)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import Maze
import Position

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

data Path = Path
  { getMaze :: Maze,
    getActions :: [Action],
    getVisited :: Set.Set Position
  }

fromMaze :: Maze -> Path
fromMaze maze = Path maze [] Set.empty

score :: Path -> Score
score path = foldr (\a s -> actionScore a + s) 0 (getActions path)
  where
    actionScore :: Action -> Score
    actionScore Forward = 1
    actionScore _ = 1_000

turnTo :: Position -> Direction -> (Position, [Action])
turnTo p direction = case getOrientation p of
  Nothing -> error "No orientation"
  Just o
    | o == direction -> (p, [])
    | clockwise o == direction -> (p', [Clockwise])
    | counterclockwise o == direction -> (p', [Counterclockwise])
    | otherwise -> (p', [Clockwise, Clockwise])
  where
    p' = p `setOrientation` Just direction

move :: Path -> Direction -> Path
move (Path maze actions visited) direction =
  let p = getPosition maze
      (turned, actions') = p `turnTo` direction
      p' = forward turned
   in Path (p' `setPosition` maze) (Forward : actions' ++ actions) (p `Set.insert` visited)

explore :: Maze -> Maybe Path
explore = explore' . fromMaze
  where
    explore' :: Path -> Maybe Path
    explore' path
      | position == getEnd maze = Just path
      | position `Set.member` getWalls maze = Nothing
      | position `Set.member` getVisited path = Nothing
      | otherwise = case getOrientation position of
          Nothing -> error "No orientation"
          Just North -> shortestPath n e w
          Just South -> shortestPath s e w
          Just East -> shortestPath e n s
          Just West -> shortestPath w n s
      where
        maze = getMaze path
        position = getPosition maze
        w = explore' (path `move` West)
        n = explore' (path `move` North)
        e = explore' (path `move` East)
        s = explore' (path `move` South)

        shortestPath :: Maybe Path -> Maybe Path -> Maybe Path -> Maybe Path
        shortestPath a b c =
          let maybePaths = a `par` b `par` c `pseq` [a, b, c]
              paths = catMaybes maybePaths
           in if null paths
                then Nothing
                else Just (minimumBy (compare `on` score) paths)

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
      (Path maze actions visited) = path
      Maze (Width (X width)) (Height (Y height)) position start end walls = maze
      visitedMap = Map.fromList (map (\p -> (Position (getX p) (getY p) Nothing, p)) (Set.toList visited))
