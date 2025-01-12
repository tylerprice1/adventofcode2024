module Maze where

import Data.Set qualified as Set
import Position

data Maze = Maze
  { getWidth :: Width,
    getHeight :: Height,
    getPosition :: Position,
    getStart :: Position,
    getEnd :: Position,
    getWalls :: Set.Set Position
  }

setPosition :: Position -> Maze -> Maze
setPosition position (Maze w h _ s end walls) = Maze w h position s end walls

instance Show Maze where
  show (Maze (Width (X width)) (Height (Y height)) position start end walls) =
    foldr
      ( \y s ->
          foldr
            ( \x s ->
                ( let p = Position (X x) (Y y) Nothing
                      ch
                        | p == position = 'O'
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
