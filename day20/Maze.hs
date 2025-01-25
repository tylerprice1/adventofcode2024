module Maze (Maze (..), getBorders, getNonWalls, getPositions, showMazeWithPath, Walls) where

import Data.Int (Int16)
import Data.Set qualified as Set
import Utils (Position)

type Walls = Set.Set Position

data Maze = Maze
  { getWidth :: Int16,
    getHeight :: Int16,
    getStart :: Position,
    getEnd :: Position,
    getWalls :: Walls
  }
  deriving (Eq, Ord)

instance Show Maze where
  show (Maze width height start end walls) =
    foldr
      ( \y s ->
          foldr
            ( \x s ->
                ( let p = (x, y)
                      ch
                        | p == start = 'S'
                        | p == end = 'E'
                        | p `Set.member` walls = '#'
                        | otherwise = '.'
                   in ch
                )
                  : s
            )
            ""
            [0 .. width - 1]
            ++ "\n"
            ++ s
      )
      ""
      [0 .. height - 1]

getBorders :: Maze -> [Position]
getBorders (Maze width height _ _ _) =
  let xs = [0 .. width - 1]
      ys = [0 .. height - 1]
   in ( foldr (\x borders -> (x, 0) : (x, height - 1) : borders) [] xs
          ++ foldr (\y borders -> (0, y) : (width - 1, y) : borders) [] ys
      )

getNonWalls :: Maze -> [Position]
getNonWalls maze =
  let walls = getWalls maze
   in filter (`Set.notMember` walls) (getPositions maze)

getPositions :: Maze -> [Position]
getPositions (Maze width height _ _ _) =
  let xs = [0 .. width - 1]
      ys = [0 .. height - 1]
   in foldr (\y ps -> foldr (\x ps -> (x, y) : ps) ps xs) [] ys

showMazeWithPath :: Maze -> [Position] -> String
showMazeWithPath (Maze width height start end walls) path =
  let pathSet = Set.fromList path
   in foldr
        ( \y s ->
            foldr
              ( \x s ->
                  ( let p = (x, y)
                        ch
                          | p == start = 'S'
                          | p == end = 'E'
                          | p `Set.member` pathSet = 'O'
                          | p `Set.member` walls = '#'
                          | otherwise = '.'
                     in ch
                  )
                    : s
              )
              ""
              [0 .. width - 1]
              ++ "\n"
              ++ s
        )
        ""
        [0 .. height - 1]
