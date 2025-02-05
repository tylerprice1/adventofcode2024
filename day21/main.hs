{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map once" #-}
-- {-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Redundant guard" #-}
{-# HLINT ignore "Use id" #-}

module Main (main) where

import Control.DeepSeq (NFData (..), deepseq)
import Data.Function (on)
import Data.List (find, group, minimumBy, sort, sortBy)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (dijkstra)
import DirectionalKeypad (DirectionalGridItem, directionalKeypad)
import GHC.Base (maxInt)
import GHC.Stack (HasCallStack)
import Grid (Grid (..), GridItem (..), isEast, isNorth, isSouth, isWest)
import Node (Node (..))
import NumericKeypad (NumericGridItem, numericKeypad)

type Key = GridItem Char

data KeyPress = KeyPress {getKey :: Key, getNumPresses :: Int}
  deriving (Eq, Ord)

instance NFData KeyPress where
  rnf (KeyPress key _) = rnf key

keyPressToKeys :: KeyPress -> [Key]
keyPressToKeys (KeyPress key n) = replicate n key

instance Node KeyPress Int where
  getEdges (KeyPress key _) = map (\(d, n) -> (d, n `KeyPress` 1)) (getEdges key)

instance Show KeyPress where
  show (KeyPress key n) = replicate n (getValue key)
  showList keys = showList (concatMap (\(KeyPress key n) -> replicate n (getValue key)) keys)

toPairs :: (HasCallStack) => [a] -> [(a, a)]
toPairs [] = []
toPairs [a] = error "Singleton"
toPairs [a, b] = [(a, b)]
toPairs (a : b : rest) = (a, b) : toPairs (b : rest)

pairToDirection :: (HasCallStack, Eq a) => (GridItem a, GridItem a) -> DirectionalGridItem
pairToDirection (from, to)
  | to `isNorth` from = u
  | to `isEast` from = r
  | to `isSouth` from = d
  | to `isWest` from = l
  | otherwise = error "Not neighbor"
  where
    [a, u, d, l, r] = directionalKeypad

toDirectionalPath :: (HasCallStack, Eq a) => [GridItem a] -> [DirectionalGridItem]
toDirectionalPath path = map pairToDirection (toPairs path)

pressKeys :: (HasCallStack) => [Key] -> [Key] -> [[[KeyPress]]]
pressKeys sequence keypad =
  foldr
    ( \(s, e) acc ->
        let paths = snd (dijkstra keypad s e maxInt)
            directionalPaths = map toDirectionalPath paths
            keyPresses = map (map (\g -> KeyPress (head g) (length g)) . group) (traceShowId directionalPaths `deepseq` directionalPaths)
            withA = map (\presses -> concatMap (\(KeyPress key n) -> [KeyPress key 1, KeyPress (head directionalKeypad) n]) presses) (traceShowId keyPresses `deepseq` keyPresses)
         in traceShowId withA `deepseq` if null acc then [withA] else concatMap (\p -> map (p :) acc) withA
    )
    []
    (toPairs sequence)

part1 sequences =
  map
    ( \numericSequence ->
        let -- numeric robot
            -- get all possible paths for each key pressed
            numericPathSequences = pressKeys numericSequence numericKeypad

            -- combine each possible path into a single list
            withAConcat = map concat numericPathSequences
            withAConcat' =
              concatMap
                ( \sequence ->
                    let -- get all possible paths for each key pressed
                        pathSequences = pressKeys (concatMap ((head directionalKeypad :) . keyPressToKeys) sequence) directionalKeypad

                        -- combine each possible path into a single list
                        withAConcat = map concat pathSequences
                     in withAConcat `deepseq` withAConcat
                )
                (withAConcat `deepseq` withAConcat)
         in sortBy (compare `on` length) withAConcat'
    )
    (take 1 sequences)

part2 input = input

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  let part1_test = part1 test
  print part1_test -- Expected: ?
  -- print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
  where
    processInput :: (HasCallStack) => String -> [[NumericGridItem]]
    processInput contents =
      let a = fromJust (find ((== 'A') . getValue) numericKeypad)
       in map
            ( (a :)
                . map
                  (\ch -> fromJust (find ((== ch) . getValue) numericKeypad))
            )
            (lines contents)
