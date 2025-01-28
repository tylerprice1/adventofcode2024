{-# LANGUAGE ScopedTypeVariables #-}

module Dijkstra (Node (getEdges), dijkstra) where

import Data.Heap qualified as Heap
import Data.Map qualified as Map
import Data.Set qualified as Set

class Node n d where
  getEdges :: n -> [(d, n)]

type PriorityQueue n d = Heap.MinPrioHeap d n

type Visited n = Set.Set n

type DistanceMap n d = Map.Map n d

type PreviousMap n = Map.Map n n

getPath :: (Eq n, Ord n) => n -> PreviousMap n -> [n]
getPath node previousMap = case Map.lookup node previousMap of
  Nothing -> [node]
  Just prev -> node : getPath prev previousMap

dijkstra ::
  forall n d.
  (Eq n, Ord n, Eq d, Ord d, Num d, Node n d) =>
  [n] ->
  n ->
  n ->
  d ->
  (d, [n])
dijkstra nodes start end maxDistance =
  let distanceMap = Map.singleton start 0
      previousMap = Map.empty

      queue :: PriorityQueue n d
      queue = foldr (\node q -> Heap.insert (if node /= start then maxDistance else 0, node) q) Heap.empty nodes

      (distanceMap', previousMap') = dijkstra' queue end Set.empty distanceMap previousMap maxDistance
      distance = Map.findWithDefault maxDistance end distanceMap'
      path = getPath end previousMap'
   in (distance, reverse path)

dijkstra' ::
  (Eq n, Ord n, Eq d, Ord d, Num d, Node n d) =>
  PriorityQueue n d ->
  n ->
  Visited n ->
  DistanceMap n d ->
  PreviousMap n ->
  d ->
  (DistanceMap n d, PreviousMap n)
dijkstra' queue end visited distanceMap previousMap maxDistance = case Heap.view queue of
  Nothing -> (distanceMap, previousMap)
  Just ((distance, node), unvisited) ->
    if distance <= maxDistance && node /= end
      then
        let edges = filter ((`Set.notMember` visited) . snd) (getEdges node)

            (unvisited', distanceMap', previousMap') =
              foldr
                ( \(edgeDistance, neighbor) (q, distMap, prevMap) ->
                    updatePosition node neighbor edgeDistance maxDistance q distMap prevMap
                )
                (unvisited, distanceMap, previousMap)
                edges
         in dijkstra' unvisited' end (node `Set.insert` visited) distanceMap' previousMap' maxDistance
      else
        (distanceMap, previousMap)

{-# INLINE updatePosition #-}
updatePosition ::
  (Eq n, Ord n, Eq d, Ord d, Num d, Node n d) =>
  n ->
  n ->
  d ->
  d ->
  PriorityQueue n d ->
  DistanceMap n d ->
  PreviousMap n ->
  (PriorityQueue n d, DistanceMap n d, PreviousMap n)
updatePosition prevNode node distance maxDistance queue distanceMap previousMap =
  if distance < Map.findWithDefault maxDistance node distanceMap
    then
      -- dist[v] ← alt
      -- prev[v] ← u
      (Heap.insert (distance, node) queue, Map.insert node distance distanceMap, Map.insert node prevNode previousMap)
    else
      (queue, distanceMap, previousMap)
