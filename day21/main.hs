{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Main (main) where

import Control.DeepSeq (deepseq)
import Data.Function (on)
import Data.List (find, intercalate, intersperse, minimumBy)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (dijkstra)
import DirectionalKeypad (DirectionalGridItem, directionalKeypad)
import GHC.Base (maxInt)
import Grid (Grid (..), GridItem (..), isEast, isNorth, isSouth, isWest)
import NumericKeypad (NumericGridItem, numericKeypad)

toPairs :: (Eq a) => [a] -> [(a, a)]
toPairs [] = []
toPairs [a] = error "Singleton"
toPairs [a, b] = [(a, b)]
toPairs (a : b : rest)
  | a == b = toPairs (b : rest)
  | otherwise = (a, b) : toPairs (b : rest)

part1 sequences =
  map
    ( \sequence ->
        foldr
          ( \(s, e) acc ->
              let paths = snd (dijkstra numericKeypad s e maxInt)
               in if null acc then [map tail paths] else concatMap (\p -> map (tail p :) acc) paths
          )
          []
          (toPairs sequence)
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
  mapM_ print (part1 test) -- Expected: ?
  -- print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
  where
    processInput :: String -> [[NumericGridItem]]
    processInput contents =
      let a = fromJust (find ((== 'A') . getValue) numericKeypad)
       in map
            ( (a :)
                . ( \line ->
                      map
                        (\ch -> fromJust (find ((== ch) . getValue) numericKeypad))
                        line
                  )
            )
            (lines contents)
