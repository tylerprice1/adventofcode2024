module Path (explore) where

import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Direction (Direction (..))
import Maze (Maze (..))
import Position (Action (..), Height (..), Position (..), Width (..), X (..), Y (..), go, setOrientation)
import Score (Score (..))

type Cache = Map.Map Position Path

data Path = Path
  { getMaze :: Maze,
    getActions :: [Action],
    getScore :: Score,
    getPosition :: Position
  }

instance Ord Path where
  compare (Path mazeA scoreA actionsA positionA) (Path mazeB scoreB actionsB positionB) =
    case compare scoreA scoreB of
      EQ -> case compare positionA positionB of
        EQ -> compare actionsA actionsB
        other -> other
      other -> other

instance Eq Path where
  (==) a b = compare a b == EQ

consAction action (Path m actions s position) = Path m (action : actions) (s + actionScore action) (position `go` action)
  where
    actionScore :: Action -> Score
    actionScore Forward = 1
    actionScore _ = 1_000

fromMaze :: Maze -> Path
fromMaze maze = Path maze [] 0 (getStart maze)

setScore :: Score -> Path -> Path
setScore score (Path m a _ p) = Path m a score p

minPath :: Path -> Path -> Path
minPath a b = if getScore a <= getScore b then a else b

minMaybePath :: Maybe Path -> Maybe Path -> Maybe Path
minMaybePath Nothing Nothing = Nothing
minMaybePath Nothing (Just b) = Just b
minMaybePath (Just a) Nothing = Just a
minMaybePath (Just a) (Just b) = Just (minPath a b)

showListPretty :: (Show a) => [a] -> String
showListPretty = foldr (\x s -> show x ++ "\n" ++ s) ""

explore :: Maze -> Maybe Path
explore maze =
  let (path, _, _) = explore' (fromMaze maze) Map.empty Infinity Set.empty 0
   in path

updateCache :: Position -> Path -> Cache -> Cache
updateCache position path cache = case position `Map.lookup` cache of
  Nothing -> Map.insert position path cache
  Just path' -> Map.insert position (minPath path path') cache

explore' :: Path -> Cache -> Score -> Set.Set Position -> Int -> (Maybe Path, Cache, Score)
explore' path cache minScore visited depth
  | depth > width * height `div` 2 = error ("Too deep " ++ "(" ++ show depth ++ ")" ++ "\n" ++ show path)
  | position == end = (Just path, updateCache end path cache, min minScore score)
  | score >= minScore = (Nothing, cache, Infinity)
  | position `Set.member` walls || position `Set.member` visited = (Nothing, cache, Infinity)
  | otherwise =
      let path' = maybe path (minPath path) (Map.lookup position cache)
       in foldr
            ( \path (maybeShortest, cache, minScore) ->
                let (path', cache', minScore') = explore' path cache minScore visited' depth'
                    maybeShortest' = minMaybePath path' maybeShortest
                 in (maybeShortest', cache', min minScore minScore')
            )
            (Nothing, updateCache position path cache, minScore)
            [ Forward `consAction` path',
              Forward `consAction` (Clockwise `consAction` path'),
              Forward `consAction` (Counterclockwise `consAction` path')
            ]
  where
    visited' = position `Set.insert` visited
    (Path maze _ score position) = path
    (Maze (Width (X width)) (Height (Y height)) _ end walls) = maze
    depthStr = replicate depth ' '
    depth' = depth + 1

instance Show Path where
  show path =
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
                          | otherwise =
                              maybe
                                '.'
                                (head . show . fromJust . getOrientation)
                                (p `Map.lookup` positionsMap)
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
      (Path maze actions score position) = path
      Maze (Width (X width)) (Height (Y height)) start end walls = maze
      positions = foldr (\action positions -> head positions `go` action : positions) [start] actions
      positionsMap = Map.fromList (map (\p -> (p `setOrientation` Nothing, p)) positions)
