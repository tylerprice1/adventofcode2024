module DirectionalKeypad (DirectionalGridItem, directionalKeypad) where

import Grid (Grid (..), GridItem (..))

type DirectionalGridItem = GridItem Char

-- -     +---+---+
-- -     | ^ | A |
-- - +---+---+---+
-- - | < | v | > |
-- - +---+---+---+
directionalKeypad :: [DirectionalGridItem]
directionalKeypad = [nA, nU, nD, nL, nR]
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
