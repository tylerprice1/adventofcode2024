module Path (explore) where

import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as Set
import Direction (Direction (..))
import GHC.Base (maxInt)
import Maze (Maze (..))
import Position (Action (..), Height (..), Position (..), Width (..), X (..), Y (..), go, move, setOrientation)
import Score (Score (..))

type Cache = Map.Map Position Path

data Path = Path
  { getMaze :: Maze,
    getActions :: [Action],
    getScore :: Score,
    getPosition :: Position
  }

instance Ord Path where
  compare (Path mazeA scoreA _ positionA) (Path mazeB scoreB _ positionB) =
    case compare scoreA scoreB of
      EQ -> case compare positionA positionB of
        EQ -> EQ
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

minimumMaybePath :: [Maybe Path] -> Maybe Path
minimumMaybePath maybePaths =
  case catMaybes maybePaths of
    [] -> Nothing
    paths -> Just (minimumBy (compare `on` getScore) paths)

showListPretty :: (Show a) => [a] -> String
showListPretty = foldr (\x s -> show x ++ "\n" ++ s) ""

explore :: Maze -> Maybe Path
explore maze =
  let (path, _, _) = explore' (fromMaze maze) Map.empty (Score maxInt) 0
   in path

explore' :: Path -> Cache -> Score -> Int -> (Maybe Path, Cache, Score)
explore' path cache minScore depth
  | depth > width * height = error ("Too deep " ++ "(" ++ show depth ++ ")" ++ "\n" ++ show path)
  | position == end = (Just path, Map.insert end path cache, min minScore score)
  | score >= minScore = (Nothing, Map.insert position (Score maxInt `setScore` path) cache, minScore)
  | position `Set.member` walls = (Nothing, cache, minScore)
  | otherwise =
      let path' = maybe path (minPath path) (Map.lookup position cache)
       in foldr
            ( \path (maybeShortest, cache, minScore) ->
                let (path', cache', minScore') = explore' path cache minScore depth'
                    maybeShortest' = minMaybePath path' maybeShortest
                 in (maybeShortest', cache', minScore')
            )
            (Nothing, cache, minScore)
            [ Forward `consAction` path',
              Forward `consAction` (Clockwise `consAction` path'),
              Forward `consAction` (Counterclockwise `consAction` path')
            ]
  where
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
      -- ++ "\n"
      -- ++ foldr (\a s -> show a ++ "\n" ++ s) "" actions

      (Path maze actions score position) = path
      Maze (Width (X width)) (Height (Y height)) start end walls = maze
      positions = foldr (\action positions -> head positions `go` action : positions) [start] actions
      positionsMap = Map.fromList (map (\p -> (p `setOrientation` Nothing, p)) positions)
