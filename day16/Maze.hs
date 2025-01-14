module Maze where

import Data.Set qualified as Set
import Position

data Maze = Maze
  { getWidth :: Width,
    getHeight :: Height,
    getStart :: Position,
    getEnd :: Position,
    getWalls :: Set.Set Position
  }
  deriving (Eq, Ord)

instance Show Maze where
  show (Maze (Width (X width)) (Height (Y height)) start end walls) =
    foldr
      ( \y s ->
          foldr
            ( \x s ->
                ( let p = Position (X x) (Y y) Nothing
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
            [1 .. width]
            ++ "\n"
            ++ s
      )
      ""
      [1 .. height]
