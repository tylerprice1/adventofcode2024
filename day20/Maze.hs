module Maze (Maze (..), getBorders, getNonWalls, getPositions, showMazeWithPath, Walls) where

import Data.List (foldl')
import Data.Set qualified as Set
import Utils (Position)

type Walls = Set.Set Position

data Maze = Maze
  { getWidth :: Int,
    getHeight :: Int,
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

{-# INLINEABLE getBorders #-}
getBorders :: Maze -> [Position]
getBorders (Maze width height _ _ _) =
  let xs = [0 .. width - 1]
      ys = [0 .. height - 1]
   in ( foldl' (\borders x -> (x, 0) : (x, height - 1) : borders) [] xs
          ++ foldl' (\borders y -> (0, y) : (width - 1, y) : borders) [] ys
      )

{-# INLINEABLE getNonWalls #-}
getNonWalls :: Maze -> [Position]
getNonWalls maze =
  let walls = getWalls maze
   in filter (`Set.notMember` walls) (getPositions maze)

{-# INLINEABLE getPositions #-}
getPositions :: Maze -> [Position]
getPositions (Maze width height _ _ _) =
  let xs = [0 .. width - 1]
      ys = [0 .. height - 1]
   in foldl (\ps y -> foldl (\ps x -> (x, y) : ps) ps xs) [] ys

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
