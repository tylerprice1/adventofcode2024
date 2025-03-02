module Journey (Journey, embark) where

import Control.Applicative ((<|>))
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as Set
import Debug.Trace (trace)
import Direction (Direction (..))
import GHC.Base (maxInt)
import Maze (Maze (Maze))
import Position (Action, Height (Height), Position (Position), Width (Width), X (X), Y (Y))
import Score (Score (..))

-- trace a b = b

type PositionJourneyCache = Map.Map Position Journey

type Visited = Set.Set Position

type CompletedJourney = (Maybe Journey, PositionJourneyCache)

data Journey
  = Journey
  { getMaze :: Maze,
    getScore :: Score,
    getActions :: [Action],
    getPath :: [Position]
  }

instance Show Journey where
  show journey =
    show score
      ++ "\n"
      ++ foldr
        ( \y s ->
            foldr
              ( \x s ->
                  ( let p = Position (X x) (Y y) Nothing
                        ch
                          | p == position && p == start = 'ß'
                          | p == position && p == end = 'Œ'
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
      -- ++ (foldr (\p s -> show p ++ "\n" ++ s) "" path)
      -- ++ (foldr (\a s -> show a ++ "\n" ++ s) "" actions)

      (Journey maze score actions path) = journey
      Maze (Width (X width)) (Height (Y height)) start end walls = maze
      pathMap = Map.fromList (map (\p -> (p `setOrientation` Nothing, p)) path)

showJourneyWithVisited journey cache visited =
  show score
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
                            Nothing
                              | p `Set.member` visited -> 'X'
                              | p `Map.member` cache -> 'C'
                              | otherwise -> '.'
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
    -- ++ (foldr (\p s -> show p ++ "\n" ++ s) "" path)
    -- ++ (foldr (\a s -> show a ++ "\n" ++ s) "" actions)

    (Journey maze score actions path) = journey
    Maze (Width (X width)) (Height (Y height)) start end walls = maze
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

nextDirections :: Position -> [Direction]
nextDirections (Position _ _ o) = case o of
  Just North -> [North, East, West]
  Just South -> [South, East, West]
  Just East -> [East, North, South]
  Just West -> [West, North, South]
  Nothing -> error "No orientation"

nextPositions :: Position -> [(Position, [Action])]
nextPositions position = map (position `move`) (nextDirections position)

embark :: Maze -> Maybe Journey
embark maze = journey
  where
    (journey, cache) = embark' (fromMaze maze) Map.empty Set.empty 0

embark' :: Journey -> PositionJourneyCache -> Visited -> Int -> CompletedJourney
embark' journey cache visited depth =
  case position `Map.lookup` cache of
    Just j ->
      let shortest = shorter journey' j
       in (Just shortest, Map.insert position shortest cache)
    Nothing
      | position == end -> trace (showJourneyWithVisited journey' cache visited) (Just journey', Map.insert position journey' cache)
      | position `Set.member` visited -> (Nothing, cache)
      | position `Set.member` walls -> (Nothing, cache)
      | depth >= width * height -> error ("Depth (" ++ show depth ++ ") is greater than area of maze")
      | otherwise ->
          let (journey'', cache') = branch journey' cache (position `Set.insert` visited) depth
           in (journey'', maybe cache' (\j -> Map.insert position j cache') journey'')
  where
    Journey maze _ _ _ = journey
    journey' = position `consPath` journey
    Maze (Width (X width)) (Height (Y height)) start end walls = maze

branch :: Journey -> PositionJourneyCache -> Visited -> Int -> CompletedJourney
branch journey cache visited depth = (foldr shorterMaybe Nothing maybeJourneys, cache')
  where
    depth' = depth + 1
    fn :: (Position, [Action]) -> ([Maybe Journey], PositionJourneyCache) -> ([Maybe Journey], PositionJourneyCache)
    fn (position, actions) (maybeJourneys, cache) =
      let (maybeJourney, cache') = embark' (actions `updateScore` journey `setMazePosition` position) cache visited depth'
       in (maybeJourney : maybeJourneys, cache')

    (maybeJourneys, cache') = foldr fn ([], cache) (nextPositions (getMazePosition journey))
