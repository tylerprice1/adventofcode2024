module Score (Score (..)) where

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)
