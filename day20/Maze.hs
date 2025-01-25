module Maze (Maze (..), getBorders, getNonBorders, getPositions, readMaze, showMazeWithPath) where

import Data.Int (Int16)
import Data.Set qualified as Set
import Utils (Position)

data Maze = Maze
  { getWidth :: Int16,
    getHeight :: Int16,
    getStart :: Position,
    getEnd :: Position,
    getWalls :: Set.Set Position,
    getNonWalls :: Set.Set Position
  }
  deriving (Eq, Ord)

readMaze :: String -> Maze
readMaze contents = maze
  where
    lns = lines contents
    height = fromIntegral (length lns)
    width = fromIntegral (length (head lns))
    maze =
      foldr
        ( \(line, y) acc ->
            foldr
              ( \(ch, x) (Maze width height start end walls nonWalls) ->
                  let !position = (x, y)
                   in case ch of
                        '#' -> Maze width height start end (position `Set.insert` walls) nonWalls
                        'S' -> Maze width height position end walls (position `Set.insert` nonWalls)
                        'E' -> Maze width height start position walls (position `Set.insert` nonWalls)
                        _ -> Maze width height start end walls (position `Set.insert` nonWalls)
              )
              acc
              (zip line [0 .. width - 1])
        )
        (Maze width height (-1, -1) (-1, -1) Set.empty Set.empty)
        (zip lns [0 .. height - 1])

instance Show Maze where
  show (Maze width height start end walls _) =
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
getBorders (Maze width height _ _ _ _) =
  let xs = [0 .. width - 1]
      ys = [0 .. height - 1]
   in ( foldr (\x borders -> (x, 0) : (x, height - 1) : borders) [] xs
          ++ foldr (\y borders -> (0, y) : (width - 1, y) : borders) [] ys
      )

{-# INLINEABLE getNonBorders #-}
getNonBorders :: Maze -> [Position]
getNonBorders maze = Set.toList (Set.difference (Set.fromList (getPositions maze)) (Set.fromList (getBorders maze)))

{-# INLINEABLE getPositions #-}
getPositions :: Maze -> [Position]
getPositions (Maze width height _ _ _ _) =
  let xs = [0 .. width - 1]
      ys = [0 .. height - 1]
   in foldr (\y ps -> foldr (\x ps -> (x, y) : ps) ps xs) [] ys

showMazeWithPath :: Maze -> [Position] -> String
showMazeWithPath (Maze width height start end walls _) path =
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
