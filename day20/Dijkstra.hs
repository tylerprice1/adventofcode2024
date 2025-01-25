module Dijkstra (dijkstra, Distance) where

import Data.Heap qualified as Heap
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Base (maxInt)
import Maze (Maze (..), getNonWalls, getPositions)
import Utils (Position, east, north, south, west)

type Distance = Int

type PriorityQueue = Heap.MinPrioHeap Distance Position

type Visited = Set.Set Position

type ScoreMap = Map.Map Position Distance

type PreviousMap = Map.Map Position Position

-- 1  S ← empty sequence
-- 2  u ← target
-- 3  if prev[u] is defined or u = source:          // Proceed if the vertex is reachable
-- 4      while u is defined:                       // Construct the shortest path with a stack S
-- 5          insert u at the beginning of S        // Push the vertex onto the stack
-- 6          u ← prev[u]
getPath :: Position -> PreviousMap -> [Position]
getPath position previousMap = case Map.lookup position previousMap of
  Nothing -> []
  Just prev -> position : getPath prev previousMap

{-# INLINEABLE dijkstra #-}
dijkstra :: Maze -> Distance -> (Distance, [Position])
dijkstra maze maxDistance =
  let Maze _ _ start end walls nonWalls = maze
      -- for each vertex v in Graph.Vertices:
      -- dist[v] ← INFINITY
      -- prev[v] ← UNDEFINED
      -- add v to Q
      -- dist[source] ← 0
      distanceMap = Map.singleton start 0
      previousMap = Map.empty

      queue :: PriorityQueue
      queue =
        foldr
          ( \p queue ->
              -- 2. Assign to every node a distance from start value: for the starting node, it is zero, and for all other nodes, it is infinity, since initially no path is known to these nodes.
              --    During execution, the distance of a node N is the length of the shortest path discovered so far between the starting node and N.[18]
              let score = if p /= start then maxInt else 0
               in Heap.insert (score, p) queue
          )
          Heap.empty
          nonWalls

      visited = walls
      (distanceMap', previousMap') = dijkstra' queue visited distanceMap previousMap
      score = (Map.!) distanceMap' end
   in if score <= maxDistance then (score, getPath end previousMap') else (maxInt, [])

dijkstra' :: PriorityQueue -> Visited -> ScoreMap -> PreviousMap -> (ScoreMap, PreviousMap)
dijkstra' queue visited distanceMap previousMap = case Heap.view queue of
  -- while Q is not empty:
  Nothing -> (distanceMap, previousMap)
  -- u ← vertex in Q with minimum dist[u]
  -- remove u from Q
  Just ((distance, position), unvisited) ->
    if distance == maxInt
      then (distanceMap, previousMap)
      else
        let newDistance = distance + 1
            -- for each neighbor v of u still in Q:
            newPositions = filter (`Set.notMember` visited) [north position, east position, south position, west position]

            -- alt ← dist[u] + Graph.Edges(u, v)
            (unvisited', distanceMap', previousMap') =
              foldr
                ( \newPosition (unvisited, distanceMap, previousMap) ->
                    updatePosition position newPosition newDistance unvisited distanceMap previousMap
                )
                (unvisited, distanceMap, previousMap)
                newPositions
         in dijkstra' unvisited' (position `Set.insert` visited) distanceMap' previousMap'

{-# INLINE updatePosition #-}
updatePosition :: Position -> Position -> Distance -> PriorityQueue -> ScoreMap -> PreviousMap -> (PriorityQueue, ScoreMap, PreviousMap)
updatePosition prevPosition position score queue scoreMap previousMap =
  if score < Map.findWithDefault maxInt position scoreMap
    then
      -- dist[v] ← alt
      -- prev[v] ← u
      (Heap.insert (score, position) queue, Map.insert position score scoreMap, Map.insert position prevPosition previousMap)
    else
      (queue, scoreMap, previousMap)
