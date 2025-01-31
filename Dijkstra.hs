module Dijkstra (dijkstra) where

import Data.Heap qualified as Heap
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Node (Node (getEdges))

type Queue n d = Heap.MinPrioHeap d n

type Visited n = Set.Set n

type DistanceMap n d = Map.Map n d

type PreviousMap n = Map.Map n [n]

getPaths :: (HasCallStack) => (Eq n, Ord n) => n -> PreviousMap n -> [[n]]
getPaths node previousMap = case Map.lookup node previousMap of
  Nothing -> [[node]]
  Just prevs -> concatMap (\prev -> map (node :) (getPaths prev previousMap)) prevs

dijkstra :: (HasCallStack) => (Eq n, Ord n, Eq d, Ord d, Num d, Node n d) => [n] -> n -> n -> d -> (d, [[n]])
dijkstra nodes start end maxDistance =
  let distanceMap = Map.singleton start 0
      previousMap = Map.empty
      queue = foldr (\node -> Heap.insert (if node /= start then maxDistance else 0, node)) Heap.empty nodes

      (distanceMap', previousMap') = dijkstra' end maxDistance queue Set.empty (distanceMap, previousMap)
      distance = Map.findWithDefault maxDistance end distanceMap'
      !paths = map reverse (getPaths end previousMap')
   in (distance, paths)

dijkstra' :: (HasCallStack) => (Eq n, Ord n, Eq d, Ord d, Num d, Node n d) => n -> d -> Queue n d -> Visited n -> (DistanceMap n d, PreviousMap n) -> (DistanceMap n d, PreviousMap n)
dijkstra' end maxDistance queue visited maps = case Heap.view queue of
  Nothing -> maps
  Just ((distance, node), unvisited) ->
    if distance <= maxDistance && node /= end
      then
        let (distanceMap, previousMap) = maps
            edges = getEdges node

            (unvisited', distanceMap', previousMap') =
              foldr
                (\(weight, neighbor) acc -> updatePosition node neighbor (distance + weight) maxDistance acc)
                (unvisited, distanceMap, previousMap)
                edges
         in dijkstra' end maxDistance unvisited' (node `Set.insert` visited) (distanceMap', previousMap')
      else
        maps

{-# INLINE updatePosition #-}
updatePosition :: (HasCallStack) => (Eq n, Ord n, Eq d, Ord d, Num d, Node n d) => n -> n -> d -> d -> (Queue n d, DistanceMap n d, PreviousMap n) -> (Queue n d, DistanceMap n d, PreviousMap n)
updatePosition prevNode node distance maxDistance (queue, distanceMap, previousMap) =
  let oldDistance = Map.findWithDefault maxDistance node distanceMap
   in case compare distance oldDistance of
        LT ->
          ( Heap.insert (distance, node) queue,
            Map.insert node distance distanceMap,
            Map.insert node [prevNode] previousMap
          )
        EQ ->
          ( queue,
            distanceMap,
            Map.insert node (prevNode : Map.findWithDefault [] node previousMap) previousMap
          )
        GT -> (queue, distanceMap, previousMap)
