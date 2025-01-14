module Direction (Direction (..), clockwise, counterclockwise) where

import GHC.Generics (Generic)

data Direction = North | South | East | West
  deriving (Eq, Generic, Ord)

instance Show Direction where
  show North = "^"
  show South = "v"
  show East = ">"
  show West = "<"

clockwise :: Direction -> Direction
clockwise North = East
clockwise East = South
clockwise South = West
clockwise West = North

counterclockwise :: Direction -> Direction
counterclockwise North = West
counterclockwise West = South
counterclockwise South = East
counterclockwise East = North
