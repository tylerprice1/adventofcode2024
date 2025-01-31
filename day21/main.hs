{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Redundant guard" #-}

module Main (main) where

import Control.DeepSeq (deepseq)
import Data.Function (on)
import Data.List (find, group, groupBy, intercalate, intersperse, minimumBy, sort, sortBy)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (dijkstra)
import DirectionalKeypad (DirectionalGridItem, directionalKeypad)
import GHC.Base (maxInt)
import GHC.Stack (HasCallStack)
import Grid (Grid (..), GridItem (..), isEast, isNorth, isSouth, isWest)
import NumericKeypad (NumericGridItem, numericKeypad)

toPairs :: (HasCallStack) => (Eq a) => [a] -> [(a, a)]
toPairs [] = []
toPairs [a] = error "Singleton"
toPairs [a, b] = [(a, b)]
toPairs (a : b : rest) = (a, b) : toPairs (b : rest)

navigate :: (HasCallStack) => [GridItem Char] -> [GridItem Char] -> [[[GridItem Char]]]
navigate sequence keypad =
  foldr
    ( \(s, e) acc ->
        let paths = snd (dijkstra keypad s e maxInt)
         in if null acc then [paths] else concatMap (\p -> map (p :) acc) paths
    )
    []
    (toPairs sequence)

toDirectionalPath :: (HasCallStack) => [GridItem Char] -> [GridItem Char]
toDirectionalPath [] = []
toDirectionalPath [only] = error ("Singleton: " ++ show only)
toDirectionalPath (a : b : rest)
  | b `isNorth` a = u : remaining
  | b `isEast` a = r : remaining
  | b `isSouth` a = d : remaining
  | b `isWest` a = l : remaining
  | otherwise = error ("Not neighbor: " ++ show (a : b : rest))
  where
    [_, u, d, l, r] = directionalKeypad
    remaining = if null rest then [] else toDirectionalPath (b : rest)

part1 sequences =
  map
    ( \sequence ->
        let -- numeric robot
            -- get all possible paths for each key pressed
            numericPathSequences = take 1 (navigate sequence numericKeypad)

            -- convert paths of positions to directions moved
            directionalNumericPathSequences = map (map toDirectionalPath) numericPathSequences

            -- press after navigating to each key
            withA = map (concatMap (\path -> path : [take 1 directionalKeypad])) directionalNumericPathSequences

            -- combine each possible path into a single list
            withAConcat = map concat withA

            -- directional robot

            -- get all possible paths for each key pressed
            directionalPathSequencess = map (take 1 . (`navigate` directionalKeypad) . (head directionalKeypad :)) withAConcat

            -- convert paths of positions to directions moved
            directionalDirectionalPathSequencess = map (map (map toDirectionalPath)) directionalPathSequencess

            withA' = map (map (concatMap (\path -> path : [take 1 directionalKeypad]))) directionalDirectionalPathSequencess

            withAConcat' = map (map concat) withA'

            sequencess = map (concatMap (`navigate` directionalKeypad)) withAConcat'
         in directionalPathSequencess
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
  print (part1 test) -- Expected: ?
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
