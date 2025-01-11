module Position where

import Data.Maybe (fromJust, isNothing)
import GHC.Generics (Generic)

newtype X = X Int deriving (Eq, Generic, Ord, Num, Show)

newtype Width = Width X deriving (Eq, Ord, Show)

newtype Y = Y Int deriving (Eq, Generic, Ord, Num, Show)

newtype Height = Height Y deriving (Eq, Ord, Show)

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

data Action = Forward | Clockwise | Counterclockwise
  deriving (Eq, Ord, Show)

data Position = Position {getX :: !X, getY :: !Y, getOrientation :: !(Maybe Direction)}
  deriving (Generic)

setOrientation :: Position -> Maybe Direction -> Position
setOrientation (Position x y _) = Position x y

north :: Position -> Position
north (Position x y _) = Position x (y - 1) (Just North)

south :: Position -> Position
south (Position x y _) = Position x (y + 1) (Just South)

west :: Position -> Position
west (Position x y _) = Position (x - 1) y (Just West)

east :: Position -> Position
east (Position x y _) = Position (x + 1) y (Just East)

forward :: Position -> Position
forward p = case fromJust (getOrientation p) of
  North -> north p
  East -> east p
  South -> south p
  West -> west p

perform :: Position -> Action -> Position
perform p Forward = forward p
perform p Clockwise = p `setOrientation` Just ((clockwise . fromJust . getOrientation) p)
perform p Counterclockwise = p `setOrientation` Just ((counterclockwise . fromJust . getOrientation) p)

instance Eq Position where
  (==) :: Position -> Position -> Bool
  (==) (Position x1 y1 o1) (Position x2 y2 o2) = x1 == x2 && y1 == y2 && (isNothing o1 || isNothing o2 || o1 == o2)

instance Ord Position where
  compare :: Position -> Position -> Ordering
  compare (Position x1 y1 o1) (Position x2 y2 o2) = case compare x1 x2 of
    EQ -> case compare y1 y2 of
      EQ -> case o1 of
        Nothing -> EQ
        (Just o1) -> case o2 of
          Nothing -> EQ
          (Just o2) -> compare o1 o2
      other -> other
    other -> other

instance Show Position where
  show :: Position -> String
  show (Position x y o) =
    "("
      ++ show x
      ++ ", "
      ++ show y
      ++ ( case o of
             Nothing -> ""
             Just o -> ", " ++ show o
         )
      ++ ")"
