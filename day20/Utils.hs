module Utils (Position, north, east, south, west) where

import Data.Int (Int16)

type Position = (Int16, Int16)

{-# INLINE north #-}
north :: Position -> Position
north (x, y) = (x, y - 1)

{-# INLINE east #-}
east :: Position -> Position
east (x, y) = (x - 1, y)

{-# INLINE south #-}
south :: Position -> Position
south (x, y) = (x, y + 1)

{-# INLINE west #-}
west :: Position -> Position
west (x, y) = (x + 1, y)
