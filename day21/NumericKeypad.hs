module NumericKeypad (NumericGridItem, numericKeypad) where

import Grid (Grid (..), GridItem (..))

type NumericGridItem = GridItem Char Int

-- - +---+---+---+
-- - | 7 | 8 | 9 |
-- - +---+---+---+
-- - | 4 | 5 | 6 |
-- - +---+---+---+
-- - | 1 | 2 | 3 |
-- - +---+---+---+
-- -     | 0 | A |
-- -     +---+---+
numericKeypad :: [NumericGridItem]
numericKeypad = [nA, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9]
  where
    edge n = (1, n)

    nA = GridItem 'A' (Just e3) Nothing Nothing (Just e0)
    n0 = GridItem '0' (Just e2) (Just eA) Nothing Nothing
    n1 = GridItem '1' (Just e4) (Just e2) Nothing Nothing
    n2 = GridItem '2' (Just e5) (Just e3) (Just e0) (Just e1)
    n3 = GridItem '3' (Just e6) Nothing (Just eA) (Just e2)
    n4 = GridItem '4' (Just e7) (Just e5) (Just e1) Nothing
    n5 = GridItem '5' (Just e8) (Just e6) (Just e2) (Just e4)
    n6 = GridItem '6' (Just e9) Nothing (Just e3) (Just e5)
    n7 = GridItem '7' Nothing (Just e8) (Just e4) Nothing
    n8 = GridItem '8' Nothing (Just e9) (Just e5) (Just e7)
    n9 = GridItem '9' Nothing Nothing (Just e6) (Just e8)

    eA = edge nA
    e0 = edge n0
    e1 = edge n1
    e2 = edge n2
    e3 = edge n3
    e4 = edge n4
    e5 = edge n5
    e6 = edge n6
    e7 = edge n7
    e8 = edge n8
    e9 = edge n9
