module Dijkstra (dijkstra, getPath, getPaths) where

import Data.Function (on)
import Data.Heap qualified as Heap
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Base (maxInt)
import Maze (Maze (..))
import Utils (Direction (..), Position, east, north, south, west)

type PriorityQueue = Heap.MinPrioHeap Int Position

type Visited = Set.Set Position

type ScoreMap = Map.Map Position Int

type PreviousMap = Map.Map Position (Maybe [Position])

-- 1  S ← empty sequence
-- 2  u ← target
-- 3  if prev[u] is defined or u = source:          // Proceed if the vertex is reachable
-- 4      while u is defined:                       // Construct the shortest path with a stack S
-- 5          insert u at the beginning of S        // Push the vertex onto the stack
-- 6          u ← prev[u]
getPath :: Position -> PreviousMap -> [Position]
getPath position previousMap = case Map.lookup position previousMap of
  Nothing -> []
  Just maybePrev -> maybe [position] (\(prev : _) -> position : getPath prev previousMap) maybePrev

getPaths :: Position -> PreviousMap -> [[Position]]
getPaths position previousMap = case Map.lookup position previousMap of
  Nothing -> []
  Just maybePrevs ->
    maybe
      [[position]]
      (concatMap (\prev -> map (position :) (prev `getPaths` previousMap)))
      maybePrevs

dijkstra :: Maze -> (Int, [[Position]])
dijkstra maze =
  let Maze width height start end walls = maze
      -- for each vertex v in Graph.Vertices:
      -- dist[v] ← INFINITY
      -- prev[v] ← UNDEFINED
      -- add v to Q
      -- dist[source] ← 0
      queue :: PriorityQueue
      scoreMap :: ScoreMap
      previousMap :: PreviousMap
      (queue, scoreMap, previousMap) =
        foldr
          ( \x acc ->
              foldr
                ( \y (queue, scoreMap, previousMap) ->
                    let p = (x, y)
                     in if p `Set.member` walls
                          then (queue, scoreMap, previousMap)
                          else
                            -- 2. Assign to every node a distance from start value: for the starting node, it is zero, and for all other nodes, it is infinity, since initially no path is known to these nodes.
                            --    During execution, the distance of a node N is the length of the shortest path discovered so far between the starting node and N.[18]
                            let score = if p == start then 0 else maxInt
                             in ( Heap.insert (score, p) queue,
                                  Map.insert p score scoreMap,
                                  Map.insert p Nothing previousMap
                                )
                )
                acc
                [0 .. height - 1]
          )
          (Heap.empty, Map.empty, Map.empty)
          [0 .. width - 1]
      --
      (scoreMap', previousMap') = dijkstra' maze queue Set.empty scoreMap previousMap
   in ((Map.!) scoreMap' end, getPaths end previousMap')

dijkstra' :: Maze -> PriorityQueue -> Visited -> ScoreMap -> PreviousMap -> (ScoreMap, PreviousMap)
dijkstra' maze unvisited visited scoreMap previousMap = case Heap.view unvisited of
  Nothing -> (scoreMap, previousMap)
  -- while Q is not empty:
  Just ((currScore, currPosition), unvisited') ->
    -- u ← vertex in Q with minimum dist[u]
    -- remove u from Q
    let ([(currScore, currPosition)], unvisited') = Heap.splitAt 1 unvisited
        visited' = currPosition `Set.insert` visited
        -- for each neighbor v of u still in Q:
        n = north currPosition
        e = east currPosition
        s = south currPosition
        w = west currPosition

        -- alt ← dist[u] + Graph.Edges(u, v)
        (nUnvisited, nScoreMap, nPreviousMap) = updatePosition currPosition n (currScore + 1) unvisited' visited' scoreMap previousMap
        (eUnvisited, eScoreMap, ePreviousMap) = updatePosition currPosition e (currScore + 1) nUnvisited visited' nScoreMap nPreviousMap
        (sUnvisited, sScoreMap, sPreviousMap) = updatePosition currPosition s (currScore + 1) eUnvisited visited' eScoreMap ePreviousMap
        (wUnvisited, wScoreMap, wPreviousMap) = updatePosition currPosition w (currScore + 1) sUnvisited visited' sScoreMap sPreviousMap
     in dijkstra' maze wUnvisited visited' wScoreMap wPreviousMap

heapMember :: Position -> PriorityQueue -> Bool
heapMember position heap
  | Heap.null heap = False
  | otherwise = case Heap.view heap of
      Nothing -> False
      Just ((_, p), heap') -> position == p || (position `heapMember` heap')

updatePosition :: Position -> Position -> Int -> PriorityQueue -> Visited -> ScoreMap -> PreviousMap -> (PriorityQueue, ScoreMap, PreviousMap)
updatePosition prevPosition position score queue visited scoreMap previousMap
  | position `Set.member` visited = (queue, scoreMap, previousMap)
  | otherwise =
      case Map.lookup position scoreMap of
        Nothing -> (queue, scoreMap, previousMap) -- wall
        Just oldScore ->
          -- if alt < dist[v]:
          if score < oldScore
            -- dist[v] ← alt
            -- prev[v] ← u
            then (Heap.insert (score, position) queue, Map.insert position score scoreMap, Map.insert position (Just [prevPosition]) previousMap)
            else
              if score == oldScore
                then
                  (queue, scoreMap, Map.insert position (Just (prevPosition : fromMaybe [] (Map.findWithDefault (Just []) position previousMap))) previousMap)
                else
                  (queue, scoreMap, previousMap)
