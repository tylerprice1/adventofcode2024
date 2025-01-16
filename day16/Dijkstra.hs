module Dijkstra (dijkstra) where

import Data.Function (on)
import Data.Heap qualified as Heap
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import Direction (Direction (..), clockwise, counterclockwise)
import Maze (Maze (..))
import Position (Action (..), Height (Height), Position (Position), Width (Width), X (X), Y (Y), forward, go, setOrientation)
import Score (Score (..))

type PriorityQueue = Heap.MinPrioHeap Score Position

type Visited = Set.Set Position

type ScoreMap = Map.Map Position Score

type PreviousMap = Map.Map Position (Maybe Position)

-- 1  S ← empty sequence
-- 2  u ← target
-- 3  if prev[u] is defined or u = source:          // Proceed if the vertex is reachable
-- 4      while u is defined:                       // Construct the shortest path with a stack S
-- 5          insert u at the beginning of S        // Push the vertex onto the stack
-- 6          u ← prev[u]
getPath :: Position -> PreviousMap -> [Position]
getPath position previousMap = case Map.lookup position previousMap of
  Nothing -> []
  Just maybePrev -> maybe [position] (\prev -> position : getPath prev previousMap) maybePrev

dijkstra :: Maze -> (Score, [Position])
dijkstra maze =
  let Maze (Width (X width)) (Height (Y height)) start end walls = maze
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
                    let p = Position (X x) (Y y) Nothing
                     in if p `Set.member` walls
                          then (queue, scoreMap, previousMap)
                          else
                            -- 2. Assign to every node a distance from start value: for the starting node, it is zero, and for all other nodes, it is infinity, since initially no path is known to these nodes.
                            --    During execution, the distance of a node N is the length of the shortest path discovered so far between the starting node and N.[18]
                            let north = p `setOrientation` Just North
                                east = p `setOrientation` Just East
                                south = p `setOrientation` Just South
                                west = p `setOrientation` Just West
                                nScore = if north == start then Score 0 else Infinity
                                eScore = if east == start then Score 0 else Infinity
                                sScore = if south == start then Score 0 else Infinity
                                wScore = if west == start then Score 0 else Infinity
                             in ( Heap.insert (nScore, north) (Heap.insert (eScore, east) (Heap.insert (sScore, south) (Heap.insert (wScore, west) queue))),
                                  Map.insert north nScore (Map.insert east eScore (Map.insert south sScore (Map.insert west wScore scoreMap))),
                                  Map.insert north Nothing (Map.insert east Nothing (Map.insert south Nothing (Map.insert west Nothing previousMap)))
                                )
                )
                acc
                [1 .. height]
          )
          (Heap.empty, Map.empty, Map.empty)
          [1 .. width]
      --
      (scoreMap', previousMap') = dijkstra' maze queue Set.empty scoreMap previousMap
      endNorth = end `setOrientation` Just North
      endEast = end `setOrientation` Just East
      endSouth = end `setOrientation` Just South
      endWest = end `setOrientation` Just West
      minEnd = minimumBy (compare `on` (Map.!) scoreMap') (traceShow (map ((Map.!) scoreMap') [end, endNorth, endEast, endSouth, endWest]) [end, endNorth, endEast, endSouth, endWest])
   in ((Map.!) scoreMap' minEnd, getPath minEnd previousMap')

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
        f = currPosition `go` Forward
        cw = currPosition `go` Clockwise
        ccw = currPosition `go` Counterclockwise

        -- alt ← dist[u] + Graph.Edges(u, v)
        (fUnvisited, fScoreMap, fPreviousMap) = updatePosition currPosition f (currScore + 1) unvisited' visited' scoreMap previousMap
        (cwUnvisited, cwScoreMap, cwPreviousMap) = updatePosition currPosition cw (currScore + 1000) fUnvisited visited' fScoreMap fPreviousMap
        (ccwUnvisited, ccwScoreMap, ccwPreviousMap) = updatePosition currPosition ccw (currScore + 1000) cwUnvisited visited' cwScoreMap cwPreviousMap
     in dijkstra' maze ccwUnvisited (visited') ccwScoreMap ccwPreviousMap

heapMember :: Position -> PriorityQueue -> Bool
heapMember position heap
  | Heap.null heap = False
  | otherwise = case Heap.view heap of
      Nothing -> False
      Just ((_, p), heap') -> position == p || (position `heapMember` heap')

updatePosition :: Position -> Position -> Score -> PriorityQueue -> Visited -> ScoreMap -> PreviousMap -> (PriorityQueue, ScoreMap, PreviousMap)
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
            then (Heap.insert (score, position) queue, Map.insert position score scoreMap, Map.insert position (Just prevPosition) previousMap)
            else (queue, scoreMap, previousMap)
