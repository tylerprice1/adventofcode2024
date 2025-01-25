module Dijkstra (dijkstra) where

import Data.Heap qualified as Heap
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Base (maxInt)
import Maze (Maze (..), Walls, getNonWalls, getPositions)
import Utils (Position, east, north, south, west)

type PriorityQueue = Heap.MinPrioHeap Int Position

type Visited = Set.Set Position

type Score = Int

type ScoreMap = Map.Map Position Score

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

dijkstra :: Maze -> (Int, [Position])
dijkstra maze =
  let Maze _ _ start end walls = maze
      -- for each vertex v in Graph.Vertices:
      -- dist[v] ← INFINITY
      -- prev[v] ← UNDEFINED
      -- add v to Q
      -- dist[source] ← 0
      scoreMap :: ScoreMap
      scoreMap = Map.singleton start 0

      previousMap :: PreviousMap
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
          (getNonWalls maze)

      visited = walls
      (scoreMap', previousMap') = dijkstra' queue visited scoreMap previousMap
      score = (Map.!) scoreMap' end
      path = getPath end previousMap'
   in if score <= maxInt then (score, path) else (maxInt, [])

dijkstra' :: PriorityQueue -> Visited -> ScoreMap -> PreviousMap -> (ScoreMap, PreviousMap)
dijkstra' queue visited scoreMap previousMap = case Heap.view queue of
  -- while Q is not empty:
  Nothing -> (scoreMap, previousMap)
  -- u ← vertex in Q with minimum dist[u]
  -- remove u from Q
  Just ((score, position), unvisited) ->
    if score == maxInt
      then (scoreMap, previousMap)
      else
        let newScore = score + 1
            -- for each neighbor v of u still in Q:
            newPositions = filter (`Set.notMember` visited) [north position, east position, south position, west position]

            -- alt ← dist[u] + Graph.Edges(u, v)
            (unvisited', scoreMap', previousMap') =
              foldr
                ( \newPosition (unvisited, scoreMap, previousMap) ->
                    updatePosition position newPosition newScore unvisited scoreMap previousMap
                )
                (unvisited, scoreMap, previousMap)
                newPositions
         in dijkstra' unvisited' (position `Set.insert` visited) scoreMap' previousMap'

updatePosition :: Position -> Position -> Score -> PriorityQueue -> ScoreMap -> PreviousMap -> (PriorityQueue, ScoreMap, PreviousMap)
updatePosition prevPosition position score queue scoreMap previousMap =
  let oldScore = Map.findWithDefault maxInt position scoreMap
   in case compare score oldScore of
        -- if alt < dist[v]:
        -- dist[v] ← alt
        -- prev[v] ← u
        LT -> (Heap.insert (score, position) queue, Map.insert position score scoreMap, Map.insert position prevPosition previousMap)
        EQ -> (queue, scoreMap, previousMap)
        GT -> (queue, scoreMap, previousMap)
