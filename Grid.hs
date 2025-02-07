module Grid (Grid (..), isNorth, isEast, isSouth, isWest) where

import Data.List (find)
import Data.Maybe (catMaybes)
import Node (Edge, Graph (getEdges))

class Grid n where
  getNorth :: n -> Maybe (Edge n Int)
  getEast :: n -> Maybe (Edge n Int)
  getSouth :: n -> Maybe (Edge n Int)
  getWest :: n -> Maybe (Edge n Int)

isNorth :: (Eq n, Grid n) => n -> n -> Bool
isNorth a b = maybe False (\(_, b) -> a == b) (getNorth b)

isEast :: (Eq n, Grid n) => n -> n -> Bool
isEast a b = maybe False (\(_, b) -> a == b) (getEast b)

isSouth :: (Eq n, Grid n) => n -> n -> Bool
isSouth a b = maybe False (\(_, b) -> a == b) (getSouth b)

isWest :: (Eq n, Grid n) => n -> n -> Bool
isWest a b = maybe False (\(_, b) -> a == b) (getWest b)
