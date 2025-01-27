module Grid
  ( Grid (..),
    GridItem (..),
    findGridItem,
    isNorth,
    isEast,
    isSouth,
    isWest,
  )
where

import Data.List (find)
import Data.Maybe (catMaybes)
import Debug.Trace (trace, traceEvent, traceStack)
import Dijkstra (Node (getEdges))

data GridItem v d = GridItem
  { getValue :: !v,
    getNorth :: Maybe (d, GridItem v d),
    getEast :: Maybe (d, GridItem v d),
    getSouth :: Maybe (d, GridItem v d),
    getWest :: Maybe (d, GridItem v d)
  }

instance (Eq v) => Eq (GridItem v d) where
  (==) (GridItem a _ _ _ _) (GridItem b _ _ _ _) = a == b

instance (Ord v) => Ord (GridItem v d) where
  compare (GridItem a _ _ _ _) (GridItem b _ _ _ _) = compare a b

instance (Show v) => Show (GridItem v d) where
  show (GridItem v _ _ _ _) = show v

instance (Show v, Show d) => (Node (GridItem v d) d) where
  getEdges (GridItem _ !n !e !s !w) = catMaybes [n, e, s, w]

{-# INLINE isNorth #-}
isNorth :: (Eq v, Eq d) => GridItem v d -> GridItem v d -> Bool
isNorth !n !g = maybe False (\(_, gi) -> n == gi) (getNorth g)

{-# INLINE isEast #-}
isEast :: (Eq v, Eq d) => GridItem v d -> GridItem v d -> Bool
isEast !n !g = maybe False (\(_, gi) -> n == gi) (getEast g)

{-# INLINE isSouth #-}
isSouth :: (Eq v, Eq d) => GridItem v d -> GridItem v d -> Bool
isSouth !n !g = maybe False (\(_, gi) -> n == gi) (getSouth g)

{-# INLINE isWest #-}
isWest :: (Eq v, Eq d) => GridItem v d -> GridItem v d -> Bool
isWest !n !g = maybe False (\(_, gi) -> n == gi) (getWest g)

newtype Grid v d = Grid [GridItem v d]

{-# INLINE findGridItem #-}
findGridItem :: (Eq v, Show v) => Grid v d -> v -> Maybe (GridItem v d)
findGridItem (Grid !items) !v = find (\(GridItem v' _ _ _ _) -> traceEvent ("find " ++ show v) (v == v')) items
