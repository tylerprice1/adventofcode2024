module Utils (Direction (..), Position, north, east, south, west) where

type Position = (Int, Int)

north :: Position -> Position
north (x, y) = (x, y - 1)

east :: Position -> Position
east (x, y) = (x - 1, y)

south :: Position -> Position
south (x, y) = (x, y + 1)

west :: Position -> Position
west (x, y) = (x + 1, y)

data Direction = North | South | East | West
  deriving (Eq, Ord)

instance Show Direction where
  show North = "^"
  show South = "v"
  show East = ">"
  show West = "<"
