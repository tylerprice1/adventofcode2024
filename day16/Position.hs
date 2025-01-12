module Position where

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

data Position = Position {getX :: X, getY :: Y, getOrientation :: Maybe Direction}
  deriving (Generic)

setOrientation :: Position -> Maybe Direction -> Position
setOrientation (Position x y _) = Position x y

forward :: Position -> Position
forward (Position _ _ Nothing) = error "No orientation"
forward (Position x y (Just North)) = Position x (y - 1) (Just North)
forward (Position x y (Just South)) = Position x (y + 1) (Just South)
forward (Position x y (Just East)) = Position (x + 1) y (Just East)
forward (Position x y (Just West)) = Position (x - 1) y (Just West)

facing :: Position -> Direction -> (Position, [Action])
facing p direction = case getOrientation p of
  Just o
    | o == direction -> (p, [])
    | clockwise o == direction -> (p', [Clockwise])
    | counterclockwise o == direction -> (p', [Counterclockwise])
    | otherwise -> (p', [Clockwise, Clockwise])
  Nothing -> error "No orientation"
  where
    p' = p `setOrientation` Just direction

move :: Position -> Direction -> (Position, [Action])
move p direction =
  let (p', actions) = p `facing` direction
   in (forward p', Forward : actions)

instance Eq Position where
  (==) :: Position -> Position -> Bool
  (==) (Position x1 y1 Nothing) (Position x2 y2 _) = x1 == x2 && y1 == y2
  (==) (Position x1 y1 _) (Position x2 y2 Nothing) = x1 == x2 && y1 == y2
  (==) (Position x1 y1 o1) (Position x2 y2 o2) = x1 == x2 && y1 == y2 && o1 == o2

instance Ord Position where
  compare :: Position -> Position -> Ordering
  compare (Position x1 y1 Nothing) (Position x2 y2 _) = case compare x1 x2 of
    EQ -> compare y1 y2
    other -> other
  compare (Position x1 y1 _) (Position x2 y2 Nothing) = case compare x1 x2 of
    EQ -> compare y1 y2
    other -> other
  compare (Position x1 y1 o1) (Position x2 y2 o2) = case compare x1 x2 of
    EQ -> case compare y1 y2 of
      EQ -> compare o1 o2
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
             Just o' -> ", " ++ show o'
         )
      ++ ")"
