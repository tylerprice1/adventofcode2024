module DirectionalKeypad (DirectionalKey (..), directionalKeypad) where

import Data.Bifunctor (second)
import Grid (Grid (..))
import GridItem (GridItem (..))
import Node (Graph (..))

newtype DirectionalKey = DirectionalKey (GridItem Char)
  deriving (Eq, Ord)

instance Show DirectionalKey where
  show (DirectionalKey gi) = show gi

instance Graph DirectionalKey Int where
  getEdges (DirectionalKey gi) = map (second DirectionalKey) (getEdges gi)

instance Grid DirectionalKey where
  getNorth (DirectionalKey gi) = (Just . second DirectionalKey) =<< getNorth gi
  getEast (DirectionalKey gi) = (Just . second DirectionalKey) =<< getEast gi
  getSouth (DirectionalKey gi) = (Just . second DirectionalKey) =<< getSouth gi
  getWest (DirectionalKey gi) = (Just . second DirectionalKey) =<< getWest gi

-- -     +---+---+
-- -     | ^ | A |
-- - +---+---+---+
-- - | < | v | > |
-- - +---+---+---+
directionalKeypad :: [DirectionalKey]
directionalKeypad = map DirectionalKey [nA, nU, nD, nL, nR]
  where
    edge node = (1, node)

    nA = GridItem 'A' Nothing Nothing (Just eR) (Just eU)
    nU = GridItem '^' Nothing (Just eA) (Just eD) Nothing
    nL = GridItem '<' Nothing (Just eD) Nothing Nothing
    nR = GridItem '>' (Just eA) Nothing Nothing (Just eD)
    nD = GridItem 'v' (Just eU) (Just eR) Nothing (Just eL)

    eA = edge nA
    eU = edge nU
    eL = edge nL
    eR = edge nR
    eD = edge nD
