module Grid
  ( Grid (..),
    GridItem (..),
    isNorth,
    isEast,
    isSouth,
    isWest,
    toNode,
    toGraph,
  )
where

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Dijkstra (Graph (..), Node (..))

data GridItem v d = GridItem
  { getValue :: v,
    getNorth :: Maybe (d, GridItem v d),
    getEast :: Maybe (d, GridItem v d),
    getSouth :: Maybe (d, GridItem v d),
    getWest :: Maybe (d, GridItem v d)
  }

instance (Eq v) => Eq (GridItem v d) where
  (==) (GridItem a _ _ _ _) (GridItem b _ _ _ _) = a == b

isNorth :: (Eq v, Eq d) => GridItem v d -> GridItem v d -> Bool
isNorth n g = maybe False (\(_, gi) -> n == gi) (getNorth g)

isEast :: (Eq v, Eq d) => GridItem v d -> GridItem v d -> Bool
isEast n g = maybe False (\(_, gi) -> n == gi) (getEast g)

isSouth :: (Eq v, Eq d) => GridItem v d -> GridItem v d -> Bool
isSouth n g = maybe False (\(_, gi) -> n == gi) (getSouth g)

isWest :: (Eq v, Eq d) => GridItem v d -> GridItem v d -> Bool
isWest n g = maybe False (\(_, gi) -> n == gi) (getWest g)

toNode :: GridItem v d -> Node v d
toNode (GridItem v n e s w) = Node v (map (second toNode) (catMaybes [n, e, s, w]))

newtype Grid v d = Grid [GridItem v d]

toGraph :: Grid v d -> Graph v d
toGraph (Grid gridItems) = Graph (map toNode gridItems)
