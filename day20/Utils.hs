module Utils (Position, north, east, south, west) where

type Position = (Int, Int)

north :: Position -> Position
north (x, !y) = (x, y - 1)

east :: Position -> Position
east (!x, y) = (x - 1, y)

south :: Position -> Position
south (x, !y) = (x, y + 1)

west :: Position -> Position
west (!x, y) = (x + 1, y)
