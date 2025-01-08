module Part2 (part2) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple (swap)
import Debug.Trace (trace, traceShow, traceShowId)
import Utils

type Box = (Position, Position)

type Boxes = Map.Map Position Box

removeBox box boxes = case box `Map.lookup` boxes of
  Nothing -> boxes
  Just (l, r) -> l `Map.delete` (r `Map.delete` boxes)

data Warehouse = Warehouse
  { height :: Height,
    width :: Width,
    boxes :: Boxes,
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
                          | position `Set.member` walls = '#'
                          | otherwise = case position `Map.lookup` boxes of
                              Nothing -> '.'
                              Just (l, r) -> if position == l then '[' else ']'
                     in ch : c
                )
                "\n"
                columns
                ++ s
          )
          ""
          rows

pushBox :: Warehouse -> Position -> Direction -> Maybe Warehouse
pushBox warehouse box direction =
  case box `Map.lookup` boxes of
    Nothing ->
      error
        ( "Not a box: "
            ++ show box
            ++ " "
            ++ show direction
            ++ "\n"
            ++ show warehouse
        )
    Just positions ->
      let (l, r) = positions
          lDestination = move l direction
          rDestination = move r direction
          newPosition = (lDestination, rDestination)
          boxesWithoutBox = (l `Map.delete` (r `Map.delete` boxes))
          newBoxes =
            Map.insert
              lDestination
              newPosition
              (Map.insert rDestination newPosition boxesWithoutBox)
          pushed = Just (Warehouse height width newBoxes walls robot)
       in if lDestination `Set.member` walls || rDestination `Set.member` walls
            then Nothing
            else case (lDestination `Map.lookup` boxesWithoutBox, rDestination `Map.lookup` boxesWithoutBox) of
              (Nothing, Nothing) -> pushed
              --
              (Just lPositions, Nothing) ->
                if overlap positions lPositions
                  then pushed
                  else (\w -> pushBox w box direction) =<< pushBox warehouse lDestination direction
              --
              (Nothing, Just rPositions) ->
                if overlap positions rPositions
                  then pushed
                  else (\w -> pushBox w box direction) =<< pushBox warehouse rDestination direction
              --
              (Just lPositions, Just rPositions) ->
                ( \w ->
                    (\w' -> pushBox w' box direction)
                      =<< if overlap positions lPositions
                        then pushed
                        else pushBox w lDestination direction
                )
                  =<< if overlap positions rPositions || overlap lPositions rPositions
                    then Just warehouse
                    else pushBox warehouse rDestination direction
  where
    (Warehouse height width boxes walls robot) = warehouse
    overlap (l1, r1) (l2, r2) = l1 == l2 || l1 == r2 || r1 == r2

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
        | nextPosition `Map.member` boxes warehouse = maybe warehouse (`updateRobot` nextPosition) (pushBox warehouse nextPosition d)
        | otherwise = updateRobot warehouse nextPosition
   in navigate warehouse' directions

processInput :: String -> (Warehouse, [Direction])
processInput contents = (warehouse, directions)
  where
    (mapLines, directions') = break (== "") (lines contents)
    directions = map charToDirection (concat (drop 1 directions'))

    lns = mapLines
    h = Y (length lns)
    w = X (2 * (length . head) lns)
    warehouse =
      foldr
        ( \(l, y) w ->
            foldr
              ( \(ch, x) w ->
                  let lPosition = Position (X x) (Y y)
                      rPosition = Position (X (x + 1)) (Y y)
                      box = (lPosition, rPosition)
                      Warehouse height width boxes walls robot = w
                   in case ch of
                        '@' -> Warehouse height width boxes walls lPosition
                        'O' -> Warehouse height width (Map.insert lPosition box (Map.insert rPosition box boxes)) walls robot
                        '#' -> Warehouse height width boxes (lPosition `Set.insert` (rPosition `Set.insert` walls)) robot
                        _ -> w
              )
              w
              (zip l [0, 2 ..])
        )
        (Warehouse h w Map.empty Set.empty (Position (X 0) (Y 0)))
        (zip lns [0 ..])

part2 contents =
  let (warehouse, directions) = processInput contents
      (Warehouse height width boxes walls robot) = navigate warehouse directions
   in sum (map (\(Position (X x) (Y y), _) -> 100 * y + x) ((Set.toList . Set.fromList) (map snd (Map.toList boxes))))
