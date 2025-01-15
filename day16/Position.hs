module Position where

import Direction (Direction (..), clockwise, counterclockwise)

newtype X = X Int
  deriving (Eq, Ord, Num, Show)

newtype Width = Width X
  deriving (Eq, Ord, Show)

newtype Y = Y Int
  deriving (Eq, Ord, Num, Show)

newtype Height = Height Y
  deriving (Eq, Ord, Show)

data Action = Forward | Clockwise | Counterclockwise
  deriving (Eq, Ord, Show)

data Position = Position {getX :: X, getY :: Y, getOrientation :: Maybe Direction}

setOrientation :: Position -> Maybe Direction -> Position
setOrientation (Position x y _) = Position x y

forward :: Position -> Position
forward (Position x y Nothing) = error "No orientation"
forward (Position x y (Just North)) = Position x (y - 1) (Just North)
forward (Position x y (Just South)) = Position x (y + 1) (Just South)
forward (Position x y (Just East)) = Position (x + 1) y (Just East)
forward (Position x y (Just West)) = Position (x - 1) y (Just West)

go :: Position -> Action -> Position
go (Position x y Nothing) _ = error "No orientation"
go p Forward = forward p
go (Position x y (Just o)) Clockwise = Position x y (Just (clockwise o))
go (Position x y (Just o)) Counterclockwise = Position x y (Just (counterclockwise o))

instance Eq Position where
  (==) :: Position -> Position -> Bool
  (==) (Position x1 y1 Nothing) (Position x2 y2 _) = x1 == x2 && y1 == y2
  (==) (Position x1 y1 _) (Position x2 y2 Nothing) = x1 == x2 && y1 == y2
  (==) (Position x1 y1 o1) (Position x2 y2 o2) = x1 == x2 && y1 == y2 && o1 == o2

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
