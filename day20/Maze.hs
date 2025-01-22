module Maze (Maze (..)) where

import Data.Set qualified as Set
import Utils (Position)

data Maze = Maze
  { getWidth :: Int,
    getHeight :: Int,
    getStart :: Position,
    getEnd :: Position,
    getWalls :: Set.Set Position
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
