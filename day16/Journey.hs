module Journey (Journey, embark) where

import Control.Applicative ((<|>))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Debug.Trace (trace)
import GHC.Base (maxInt)
import Maze (Maze (Maze, getEnd, getPosition, getStart), setPosition)
import Position (Action (Forward), Direction (..), Height (..), Position (..), Width (..), X (..), Y (..), getOrientation, move, setOrientation)
import Score (Score (..))

type PositionPathCache = Map.Map Position Journey

type Visited = Set.Set Position

type CompletedJourney = (Maybe Journey, PositionPathCache)

data Journey
  = Journey
  { getMaze :: Maze,
    getScore :: Score,
    getActions :: [Action],
    getPath :: [Position]
  }

calculateScore :: Journey -> Score
calculateScore journey = foldr (\a s -> actionScore a + s) 0 (getActions journey)
  where
    actionScore :: Action -> Score
    actionScore Forward = 1
    actionScore _ = 1_000

instance Show Journey where
  show journey =
    show score
      ++ " == "
      ++ show (calculateScore journey)
      ++ "\n"
      ++ foldr
        ( \y s ->
            foldr
              ( \x s ->
                  ( let p = Position (X x) (Y y) Nothing
                        ch
                          | p == position = 'O'
                          | p == start = 'S'
                          | p == end = 'E'
                          | p `Set.member` walls = '#'
                          | otherwise = case p `Map.lookup` pathMap of
                              Nothing -> '.'
                              Just p' -> maybe 'O' (head . show) (getOrientation p')
                     in ch
                  )
                    : s
              )
              ""
              [1 .. width]
              ++ "\n"
              ++ s
        )
        ""
        [1 .. height]
    where
      (Journey maze score _ path) = journey
      Maze (Width (X width)) (Height (Y height)) position start end walls = maze
      pathMap = Map.fromList (map (\p -> (p `setOrientation` Nothing, p)) path)

fromMaze :: Maze -> Journey
fromMaze maze = Journey maze 0 [] []

updateScore :: [Action] -> Journey -> Journey
updateScore as (Journey maze score actions path) =
  let actionsScore = foldr (\a s -> s + (if a == Forward then 1 else 1000)) 0 as
   in Journey maze (score + actionsScore) (as ++ actions) path

consPath :: Position -> Journey -> Journey
consPath position (Journey m s as p) = Journey m s as (position : p)

getMazePosition :: Journey -> Position
getMazePosition (Journey m _ _ _) = getPosition m

setMazePosition :: Journey -> Position -> Journey
setMazePosition (Journey maze score as path) position = Journey (position `setPosition` maze) score as path

shorter :: Journey -> Journey -> Journey
shorter a b = if getScore a <= getScore b then a else b

shorterMaybe :: Maybe Journey -> Maybe Journey -> Maybe Journey
shorterMaybe Nothing Nothing = Nothing
shorterMaybe (Just a) Nothing = Just a
shorterMaybe Nothing (Just b) = Just b
shorterMaybe (Just a) (Just b) = Just (shorter a b)

embark :: Maze -> Maybe Journey
embark maze = trace (show (Map.size cache)) (journey)
  where
    (journey, cache) = embark' (fromMaze maze) Map.empty Set.empty 0

embark' :: Journey -> PositionPathCache -> Visited -> Int -> CompletedJourney
embark' journey positionJourneyMap visited depth
  | depth >= 25000 = error "Too deep"
  | position == end =
      let journey' = Journey maze 0 [] [end]
       in (Just journey', positionJourneyMap)
  | position `Set.member` visited = (Nothing, positionJourneyMap)
  | position `Set.member` walls = (Nothing, positionJourneyMap)
  | otherwise = case position `Map.lookup` positionJourneyMap of
      Just j -> (Just j, positionJourneyMap)
      Nothing ->
        let (journey'', positionJourneyMap') = branch journey positionJourneyMap (position `Set.insert` visited) depth
            journey' = Just . (position `consPath`) =<< journey''
         in (journey', maybe positionJourneyMap' (\j -> Map.insert position j positionJourneyMap') journey')
  where
    Journey maze _ _ _ = journey
    Maze _ _ position _ end walls = maze

nextDirections :: Position -> [Direction]
nextDirections (Position _ _ o) = case o of
  Just North -> [North, East, West]
  Just South -> [South, East, West]
  Just East -> [East, North, South]
  Just West -> [West, North, South]
  Nothing -> error "No orientation"

nextPositions :: Position -> [(Position, [Action])]
nextPositions position = map (position `move`) (nextDirections position)

branch :: Journey -> PositionPathCache -> Visited -> Int -> CompletedJourney
branch journey positionJourneyMap visited depth = (foldr shorterMaybe Nothing maybeJourneys, positionJourneyMap')
  where
    position = getMazePosition journey
    depth' = depth + 1
    fn :: (Position, [Action]) -> ([Maybe Journey], PositionPathCache) -> ([Maybe Journey], PositionPathCache)
    fn (movedPosition, actions) (maybeJourneys, map) =
      let movedJourneyStart = journey `setMazePosition` movedPosition
          (maybeMovedJourneyEnd', map') = embark' movedJourneyStart map visited depth'
          maybeMovedJourneyEnd = (\j -> Just (actions `updateScore` j)) =<< maybeMovedJourneyEnd'
       in (maybeMovedJourneyEnd : maybeJourneys, map')

    (maybeJourneys, positionJourneyMap') = foldr fn ([], positionJourneyMap) (nextPositions position)
