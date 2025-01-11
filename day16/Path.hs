module Path where

import Control.Applicative ((<|>))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import Maze
import Position

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

data Path = Path
  { getMaze :: Maze,
    getScore :: Score,
    getPath :: [Action],
    getVisited :: Set.Set Position
  }

fromMaze :: Maze -> Path
fromMaze maze = Path maze 0 [] Set.empty

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
move (Path maze score path visited) direction =
  let p = getPosition maze
      (turned, actions) = p `turnTo` direction
      p' = forward turned
   in Path (p' `setPosition` maze) score (Forward : actions ++ path) (p `Set.insert` visited)

explore :: Maze -> Maybe Path
explore = explore' . fromMaze
  where
    explore' :: Path -> Maybe Path
    explore' path
      | position == getEnd maze = (Just path)
      | position `Set.member` getWalls maze = Nothing
      | position `Set.member` getVisited path = Nothing
      | otherwise = case getOrientation position of
          Nothing -> error "No orientation"
          Just North -> n <|> e <|> w <|> s
          Just South -> s <|> e <|> w <|> n
          Just East -> e <|> n <|> s <|> w
          Just West -> w <|> n <|> s <|> e
      where
        maze = getMaze path
        position = getPosition maze
        w = explore' (path `move` West)
        n = explore' (path `move` North)
        e = explore' (path `move` East)
        s = explore' (path `move` South)

instance Show Path where
  show (Path maze score actions visited) =
    show score
      ++ "\n"
      ++ show actions
      ++ "\n"
      ++ show visited
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
                          | p `Map.member` visitedMap = maybe 'O' (\p -> maybe 'O' (head . show) (getOrientation p)) (p `Map.lookup` visitedMap)
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
      visitedMap = Map.fromList (map (\p -> (Position (getX p) (getY p) Nothing, p)) (Set.elems visited))
