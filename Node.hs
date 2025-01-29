module Node (Edge, Node (getEdges)) where

type Edge n d = (d, n)

class Node n d where
  getEdges :: n -> [Edge n d]
