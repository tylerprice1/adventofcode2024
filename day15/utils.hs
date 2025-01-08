module Utils (X (..), Y (..), Height (..), Width (..), Direction (..), Position (..), charToDirection) where

newtype X = X Int

instance Num X where
  (+) (X a) (X b) = X (a + b)
  (-) (X a) (X b) = X (a - b)
  (*) (X a) (X b) = X (a * b)
  abs (X a) = X (abs a)
  signum (X a) = X (signum a)
  fromInteger i = X (fromInteger i)
  negate (X a) = X (negate a)

instance Enum X where
  fromEnum (X x) = fromEnum x
  toEnum = X

instance Eq X where (==) (X a) (X b) = a == b

instance Ord X where (<=) (X a) (X b) = a <= b

instance Show X where show (X x) = show x

newtype Y = Y Int

instance Eq Y where (==) (Y a) (Y b) = a == b

instance Ord Y where (<=) (Y a) (Y b) = a <= b

instance Show Y where show (Y y) = show y

instance Enum Y where
  fromEnum (Y y) = fromEnum y
  toEnum = Y

instance Num Y where
  (+) (Y a) (Y b) = Y (a + b)
  (-) (Y a) (Y b) = Y (a - b)
  (*) (Y a) (Y b) = Y (a * b)
  abs (Y a) = Y (abs a)
  signum (Y a) = Y (signum a)
  fromInteger i = Y (fromInteger i)
  negate (Y a) = Y (negate a)

type Height = Y

type Width = X

data Direction = U | D | L | R
  deriving (Show)

charToDirection '^' = U
charToDirection '<' = L
charToDirection '>' = R
charToDirection 'v' = D
charToDirection ch = error ("Invalid char: " ++ [ch])

data Position = Position {x :: X, y :: Y}
  deriving (Eq)

instance Ord Position where
  compare a b = case compare (x a) (x b) of
    EQ -> compare (y a) (y b)
    other -> other

instance Show Position where
  show (Position x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
