{-# LANGUAGE ScopedTypeVariables #-}

module Dijkstra (Graph (..), Node (..), dijkstra) where

import Data.Heap qualified as Heap
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Base (maxInt)

data Node v d = Node {getValue :: v, getEdges :: [(d, Node v d)]}

instance (Eq v, Eq d) => Eq (Node v d) where
  (==) (Node aValue aEdges) (Node bValue bEdges) = aValue == bValue && aEdges == bEdges

instance (Ord v, Ord d) => Ord (Node v d) where
  compare (Node aValue aEdges) (Node bValue bEdges) = case compare aValue bValue of
    EQ -> compare aEdges bEdges
    other -> other

newtype Graph v d = Graph [Node v d]

type PriorityQueue v d = Heap.MinPrioHeap d (Node v d)

type Visited v d = Set.Set (Node v d)

type DistanceMap v d = Map.Map (Node v d) d

type PreviousMap v d = Map.Map (Node v d) (Node v d)

-- 1  S ← empty sequence
-- 2  u ← target
-- 3  if prev[u] is defined or u = source:          // Proceed if the vertex is reachable
-- 4      while u is defined:                       // Construct the shortest path with a stack S
-- 5          insert u at the beginning of S        // Push the vertex onto the stack
-- 6          u ← prev[u]
getPath :: forall v d. (Eq v, Ord v, Eq d, Ord d, Num d) => Node v d -> PreviousMap v d -> [Node v d]
getPath node previousMap = case Map.lookup node previousMap of
  Nothing -> []
  Just prev -> node : getPath prev previousMap

dijkstra :: forall v d. (Eq v, Ord v, Eq d, Ord d, Num d) => Graph v d -> Node v d -> Node v d -> (d, [Node v d])
dijkstra (Graph nodes) start end =
  let -- for each vertex v in Graph.Vertices:
      -- dist[v] ← INFINITY
      -- prev[v] ← UNDEFINED
      -- add v to Q
      -- dist[source] ← 0
      initialDistance = fromIntegral maxInt

      distanceMap :: DistanceMap v d
      distanceMap = Map.singleton start 0

      previousMap :: PreviousMap v d
      previousMap = Map.empty

      queue :: PriorityQueue v d
      queue =
        foldr
          ( \p queue ->
              -- 2. Assign to every node a distance from start value: for the starting node, it is zero, and for all other nodes, it is infinity, since initially no path is known to these nodes.
              --    During execution, the distance of a node N is the length of the shortest path discovered so far between the starting node and N.[18]
              let distance = if p /= start then initialDistance else 0
               in Heap.insert (distance, p) queue
          )
          Heap.empty
          nodes

      (distanceMap', previousMap') = dijkstra' queue end Set.empty distanceMap previousMap initialDistance
      distance = Map.findWithDefault initialDistance end distanceMap'
   in (distance, getPath end previousMap')

dijkstra' ::
  (Eq v, Ord v, Eq d, Ord d, Num d) =>
  PriorityQueue v d ->
  Node v d ->
  Visited v d ->
  DistanceMap v d ->
  PreviousMap v d ->
  d ->
  (DistanceMap v d, PreviousMap v d)
dijkstra' queue end visited distanceMap previousMap maxDistance = case Heap.view queue of
  -- while Q is not empty:
  Nothing -> (distanceMap, previousMap)
  -- u ← vertex in Q with minimum dist[u]
  -- remove u from Q
  Just ((distance, node), unvisited) ->
    if distance <= maxDistance && node /= end
      then
        let -- for each neighbor v of u still in Q:
            edges = getEdges node

            -- alt ← dist[u] + Graph.Edges(u, v)
            (unvisited', distanceMap', previousMap') =
              foldr
                ( \(edgeDistance, neighbor) (unvisited, distanceMap, previousMap) ->
                    updatePosition node neighbor edgeDistance maxDistance unvisited distanceMap previousMap
                )
                (unvisited, distanceMap, previousMap)
                edges
         in dijkstra' unvisited' end (node `Set.insert` visited) distanceMap' previousMap' maxDistance
      else
        (distanceMap, previousMap)

{-# INLINE updatePosition #-}
updatePosition ::
  (Eq v, Ord v, Eq d, Ord d, Num d) =>
  Node v d ->
  Node v d ->
  d ->
  d ->
  PriorityQueue v d ->
  DistanceMap v d ->
  PreviousMap v d ->
  (PriorityQueue v d, DistanceMap v d, PreviousMap v d)
updatePosition prevNode node distance maxDistance queue distanceMap previousMap =
  if distance < Map.findWithDefault maxDistance node distanceMap
    then
      -- dist[v] ← alt
      -- prev[v] ← u
      (Heap.insert (distance, node) queue, Map.insert node distance distanceMap, Map.insert node prevNode previousMap)
    else
      (queue, distanceMap, previousMap)
