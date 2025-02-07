module GridItem (GridItem (..), getValue) where

import Data.Maybe (catMaybes)
import Grid (Grid (..))
import Node (Edge, Graph (..))

type GridItemEdge v = Edge (GridItem v) Int

data GridItem v = GridItem v (Maybe (GridItemEdge v)) (Maybe (GridItemEdge v)) (Maybe (GridItemEdge v)) (Maybe (GridItemEdge v))

getValue :: GridItem v -> v
getValue (GridItem v _ _ _ _) = v

instance Grid (GridItem v) where
  getNorth (GridItem _ n _ _ _) = n
  getEast (GridItem _ _ e _ _) = e
  getSouth (GridItem _ _ _ s _) = s
  getWest (GridItem _ _ _ _ w) = w

instance (Graph (GridItem v) Int) where
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
