module Grid (Grid (..), GridItem (..), isNorth, isEast, isSouth, isWest) where

import Data.List (find)
import Data.Maybe (catMaybes)
import Node (Node (getEdges))

newtype Grid v = Grid [GridItem v]

type Edge v = (Int, GridItem v)

data GridItem v = GridItem {getValue :: v, getNorth :: Maybe (Edge v), getEast :: Maybe (Edge v), getSouth :: Maybe (Edge v), getWest :: Maybe (Edge v)}

{-# INLINE isNorth #-}
isNorth :: (Eq v) => GridItem v -> GridItem v -> Bool
isNorth n (GridItem _ north _ _ _) = maybe False (\(_, gi) -> n == gi) north

{-# INLINE isEast #-}
isEast :: (Eq v) => GridItem v -> GridItem v -> Bool
isEast n (GridItem _ _ east _ _) = maybe False (\(_, gi) -> n == gi) east

{-# INLINE isSouth #-}
isSouth :: (Eq v) => GridItem v -> GridItem v -> Bool
isSouth n (GridItem _ _ _ south _) = maybe False (\(_, gi) -> n == gi) south

{-# INLINE isWest #-}
isWest :: (Eq v) => GridItem v -> GridItem v -> Bool
isWest n (GridItem _ _ _ _ west) = maybe False (\(_, gi) -> n == gi) west

instance (Node (GridItem v) Int) where
  getEdges (GridItem _ n e s w) = catMaybes [n, e, s, w]

instance (Eq v) => Eq (GridItem v) where
  (==) (GridItem a _ _ _ _) (GridItem b _ _ _ _) = a == b
  (/=) (GridItem a _ _ _ _) (GridItem b _ _ _ _) = a /= b

instance (Ord v) => Ord (GridItem v) where
  compare (GridItem a _ _ _ _) (GridItem b _ _ _ _) = compare a b
  (<) (GridItem a _ _ _ _) (GridItem b _ _ _ _) = a < b
  (<=) (GridItem a _ _ _ _) (GridItem b _ _ _ _) = a <= b
  (>) (GridItem a _ _ _ _) (GridItem b _ _ _ _) = a > b
  (>=) (GridItem a _ _ _ _) (GridItem b _ _ _ _) = a >= b

instance (Show v) => Show (GridItem v) where
  show = show . getValue
  showList = showList . map getValue
