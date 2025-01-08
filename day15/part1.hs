module Part1 (part1) where

import Data.Set qualified as Set
import Utils

data Warehouse = Warehouse
  { height :: Height,
    width :: Width,
    boxes :: Set.Set Position,
    walls :: Set.Set Position,
    robot :: Position
  }

updateRobot (Warehouse height width boxes walls _) = Warehouse height width boxes walls

instance Show Warehouse where
  show :: Warehouse -> String
  show (Warehouse height width boxes walls robot) =
    let rows = [(Y 0) .. height - 1]
        columns = [(X 0) .. width - 1]
     in foldr
          ( \y s ->
              foldr
                ( \x c ->
                    let position = Position x y
                        ch
                          | position == robot = '@'
                          | position `Set.member` boxes = 'O'
                          | position `Set.member` walls = '#'
                          | otherwise = '.'
                     in ch : c
                )
                "\n"
                columns
                -- ++ "\n"
                ++ s
          )
          ""
          rows

pushBox :: Warehouse -> Position -> Direction -> Maybe Warehouse
pushBox warehouse position direction
  | not (position `Set.member` boxes) = error "Not a box"
  | destination `Set.member` walls = Nothing
  | destination `Set.member` boxes = (\w -> pushBox w position direction) =<< pushBox warehouse destination direction
  | otherwise = Just (Warehouse height width (destination `Set.insert` (position `Set.delete` boxes)) walls robot)
  where
    destination = move position direction
    (Warehouse height width boxes walls robot) = warehouse

move position direction = case direction of
  U -> Position (x position) (y position - 1)
  L -> Position (x position - 1) (y position)
  R -> Position (x position + 1) (y position)
  D -> Position (x position) (y position + 1)

navigate :: Warehouse -> [Direction] -> Warehouse
navigate warehouse [] = warehouse
navigate warehouse (d : directions) =
  let position = robot warehouse
      nextPosition = move position d
      warehouse'
        | nextPosition `Set.member` walls warehouse = warehouse
        | nextPosition `Set.member` boxes warehouse = maybe warehouse (`updateRobot` nextPosition) (pushBox warehouse nextPosition d)
        | otherwise = updateRobot warehouse nextPosition
   in navigate warehouse' directions

processInput :: String -> (Warehouse, [Direction])
processInput contents = (warehouse, directions)
  where
    (mapLines, directions') = break (== "") (lines contents)
    directions = map charToDirection (concat (drop 1 directions'))

    lns = mapLines
    h = Y (length lns)
    w = X ((length . head) lns)
    warehouse =
      foldr
        ( \(l, r) w ->
            foldr
              ( \(ch, c) w ->
                  let position = Position (X c) (Y r)
                      Warehouse height width boxes walls robot = w
                   in case ch of
                        '@' -> Warehouse height width boxes walls position
                        'O' -> Warehouse height width (position `Set.insert` boxes) walls robot
                        '#' -> Warehouse height width boxes (position `Set.insert` walls) robot
                        _ -> w
              )
              w
              (zip l [0 ..])
        )
        (Warehouse h w Set.empty Set.empty (Position (X 0) (Y 0)))
        (zip lns [0 ..])

part1 contents =
  let (warehouse, directions) = processInput contents
      (Warehouse height width boxes walls robot) = navigate warehouse directions
   in sum (map (\(Position (X x) (Y y)) -> 100 * y + x) (Set.toList boxes))
