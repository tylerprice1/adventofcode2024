module Grid
  ( Grid,
    GridItem,
    Height,
    Width,
    Row,
    Column,
    Position,
    fromList,
    toList,
    height,
    width,
    hasPosition,
    getPosition,
    surrounding,
    value,
    up,
    left,
    right,
    down,
  )
where

import Data.Maybe (catMaybes)
import Data.Vector qualified as Vector

type Height = Int

type Width = Int

type Row = Int

type Column = Int

type Position = (Row, Column)

data GridItem a = GridItem
  { value :: a,
    row :: Int,
    column :: Int,
    up :: Maybe (GridItem a),
    left :: Maybe (GridItem a),
    right :: Maybe (GridItem a),
    down :: Maybe (GridItem a)
  }

instance (Show a) => Show (GridItem a) where
  -- show i = show (value i)
  show i = show (row i, column i, value i)

instance (Eq a) => Eq (GridItem a) where
  (==) a b = row a == row b && column a == column b

instance (Ord a) => Ord (GridItem a) where
  (<=) a b = row a <= row b && column a <= column b

type Grid a = (Vector.Vector (Vector.Vector (GridItem a)))

hasPosition :: Grid a -> Position -> Bool
hasPosition grid (r, c) =
  let h = height grid
      w = width grid
   in r >= 0 && r < h && c >= 0 && c < w

getPosition :: Grid a -> Position -> Maybe (GridItem a)
getPosition grid (r, c) = case (Vector.!?) grid r of
  Nothing -> Nothing
  Just row -> (Vector.!?) row c

fromList :: [[a]] -> Grid a
fromList rows =
  let unlinkedNodes =
        Vector.fromList
          ( map
              ( Vector.fromList
                  . map
                    ( \column ->
                        GridItem
                          { value = column,
                            row = 0,
                            column = 0,
                            up = Nothing,
                            down = Nothing,
                            left = Nothing,
                            right = Nothing
                          }
                    )
              )
              rows
          )

      linkedNodes =
        Vector.imap
          ( \r row ->
              Vector.imap
                ( \c gridItem ->
                    GridItem
                      { value = value gridItem,
                        row = r,
                        column = c,
                        up = getPosition linkedNodes (r - 1, c),
                        down = getPosition linkedNodes (r + 1, c),
                        left = getPosition linkedNodes (r, c - 1),
                        right = getPosition linkedNodes (r, c + 1)
                      }
                )
                row
          )
          unlinkedNodes
   in linkedNodes

toList :: Grid a -> [[a]]
toList g = Vector.toList (Vector.map (Vector.toList . Vector.map value) g)

height :: Grid a -> Int
height = Vector.length

width :: Grid a -> Int
width = Vector.length . Vector.head

surrounding :: GridItem a -> [GridItem a]
surrounding i = catMaybes [left i, up i, right i, down i]
