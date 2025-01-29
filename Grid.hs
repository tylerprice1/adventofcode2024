module Grid (Grid (..), GridItem (..), isNorth, isEast, isSouth, isWest) where

import Data.List (find)
import Data.Maybe (catMaybes)
import Node (Node (getEdges))

newtype Grid v = Grid [GridItem v]

data GridItem v = GridItem
  { getValue :: !v,
    getNorth :: Maybe (Int, GridItem v),
    getEast :: Maybe (Int, GridItem v),
    getSouth :: Maybe (Int, GridItem v),
    getWest :: Maybe (Int, GridItem v)
  }

instance (Node (GridItem v) Int) where
  getEdges (GridItem _ n e s w) = catMaybes [n, e, s, w]

instance (Eq v) => Eq (GridItem v) where
  (==) (GridItem a _ _ _ _) (GridItem b _ _ _ _) = a == b

instance (Ord v) => Ord (GridItem v) where
  compare (GridItem a _ _ _ _) (GridItem b _ _ _ _) = compare a b

instance (Show v) => Show (GridItem v) where
  show (GridItem v _ _ _ _) = show v

{-# INLINE isNorth #-}
isNorth :: (Eq v) => GridItem v -> GridItem v -> Bool
isNorth !n !g = maybe False (\(_, gi) -> n == gi) (getNorth g)

{-# INLINE isEast #-}
isEast :: (Eq v) => GridItem v -> GridItem v -> Bool
isEast !n !g = maybe False (\(_, gi) -> n == gi) (getEast g)

{-# INLINE isSouth #-}
isSouth :: (Eq v) => GridItem v -> GridItem v -> Bool
isSouth !n !g = maybe False (\(_, gi) -> n == gi) (getSouth g)

{-# INLINE isWest #-}
isWest :: (Eq v) => GridItem v -> GridItem v -> Bool
isWest !n !g = maybe False (\(_, gi) -> n == gi) (getWest g)
