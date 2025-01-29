{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.DeepSeq (deepseq)
import Data.Function (on)
import Data.List (find, intercalate, intersperse, minimumBy)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (dijkstra)
import DirectionalKeypad (DirectionalGridItem, directionalKeypad)
import GHC.Base (maxInt)
import GHC.List (foldl')
import Grid (Grid (..), GridItem (..), findGridItem, isEast, isNorth, isSouth, isWest)
import NumericKeypad (NumericGridItem, numericKeypad)

type Path a = [a]

type PathSequence a = [Path a]

toPairs :: (Eq a) => [a] -> [(a, a)]
toPairs [] = []
toPairs [a] = error "Singleton"
toPairs [a, b] = [(a, b)]
toPairs (a : b : rest) = if a /= b then (a, b) : toPairs (b : rest) else toPairs (b : rest)

-- part1 :: ([String], NumericKeypad, DirectionalKeypad) -> [[[[PathSequence DirectionalGridItem]]]]
part1 sequences =
  map
    ( \sequence -> toPairs (fromJust (find ((== 'A') . getValue) numericKeypad) : sequence)
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

processInput :: String -> [[NumericGridItem]]
processInput contents = map (map (\ch -> fromJust (find ((== ch) . getValue) numericKeypad))) (lines contents)
