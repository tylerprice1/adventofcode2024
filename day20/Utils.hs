module Utils (Position, north, east, south, west) where

type Position = (Int, Int)

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
