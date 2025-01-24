module Dijkstra (dijkstra) where

import Data.Heap qualified as Heap
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Base (maxInt)
import Maze (Maze (..))
import Utils (Position, east, north, south, west)

type PriorityQueue = Heap.MinPrioHeap Int Position

type Visited = Set.Set Position

type ScoreMap = Map.Map Position Int

type PreviousMap = Map.Map Position [Position]

-- 1  S ← empty sequence
-- 2  u ← target
-- 3  if prev[u] is defined or u = source:          // Proceed if the vertex is reachable
-- 4      while u is defined:                       // Construct the shortest path with a stack S
-- 5          insert u at the beginning of S        // Push the vertex onto the stack
-- 6          u ← prev[u]
getPath :: Position -> PreviousMap -> [Position]
getPath !position !previousMap = case Map.lookup position previousMap of
  Nothing -> []
  Just [] -> [position]
  Just (prev : _) -> position : getPath prev previousMap

getPaths :: Position -> PreviousMap -> [[Position]]
getPaths !position !previousMap = case Map.lookup position previousMap of
  Nothing -> []
  Just [] -> [[position]]
  Just prevs -> concatMap (\prev -> map (position :) (prev `getPaths` previousMap)) prevs

dijkstra :: Maze -> (Int, [Position])
dijkstra !maze =
  let Maze width height start end walls = maze
      -- for each vertex v in Graph.Vertices:
      -- dist[v] ← INFINITY
      -- prev[v] ← UNDEFINED
      -- add v to Q
      -- dist[source] ← 0
      queue :: PriorityQueue
      scoreMap :: ScoreMap
      previousMap :: PreviousMap
      (!queue, !scoreMap, previousMap) =
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
                            let score = if p /= start then maxInt else 0
                             in ( Heap.insert (score, p) queue,
                                  Map.insert p score scoreMap,
                                  Map.insert p [] previousMap
                                )
                )
                acc
                [0 .. height - 1]
          )
          (Heap.empty, Map.empty, Map.empty)
          [0 .. width - 1]
      --
      (scoreMap', previousMap') = dijkstra' maze queue Set.empty scoreMap previousMap
      score = (Map.!) scoreMap' end
      path = getPath end previousMap'
   in if score <= maxInt then (score, path) else (maxInt, [])

dijkstra' :: Maze -> PriorityQueue -> Visited -> ScoreMap -> PreviousMap -> (ScoreMap, PreviousMap)
dijkstra' !maze !unvisited !visited scoreMap previousMap = case Heap.view unvisited of
  Nothing -> (scoreMap, previousMap)
  -- while Q is not empty:
  Just ((!currScore, !currPosition), unvisited) ->
    -- u ← vertex in Q with minimum dist[u]
    -- remove u from Q
    let newVisited = currPosition `Set.insert` visited
        !newScore = currScore + 1
        -- for each neighbor v of u still in Q:
        !n = north currPosition
        !e = east currPosition
        !s = south currPosition
        !w = west currPosition

        -- alt ← dist[u] + Graph.Edges(u, v)
        (unvisited', scoreMap', previousMap') =
          foldr
            ( \newPosition (unvisited, scoreMap, previousMap) ->
                updatePosition currPosition newPosition newScore unvisited newVisited scoreMap previousMap
            )
            (unvisited, scoreMap, previousMap)
            [n, e, s, w]
     in if currScore < maxInt
          then dijkstra' maze unvisited' newVisited scoreMap' previousMap'
          else (scoreMap, previousMap)

updatePosition :: Position -> Position -> Int -> PriorityQueue -> Visited -> ScoreMap -> PreviousMap -> (PriorityQueue, ScoreMap, PreviousMap)
updatePosition prevPosition !position score queue !visited scoreMap previousMap
  | position `Set.member` visited = (queue, scoreMap, previousMap)
  | otherwise =
      case Map.lookup position scoreMap of
        Nothing -> (queue, scoreMap, previousMap) -- wall
        Just !oldScore ->
          -- if alt < dist[v]:
          if score < oldScore
            -- dist[v] ← alt
            -- prev[v] ← u
            then (Heap.insert (score, position) queue, Map.insert position score scoreMap, Map.insert position [prevPosition] previousMap)
            else
              if score == oldScore
                then
                  (queue, scoreMap, Map.insert position (prevPosition : Map.findWithDefault [] position previousMap) previousMap)
                else
                  (queue, scoreMap, previousMap)
