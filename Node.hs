module Node (Edge, Graph (..)) where

type Edge n d = (d, n)

class Graph n d where
  getEdges :: n -> [Edge n d]
