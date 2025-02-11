{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map once" #-}
-- {-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Redundant guard" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Fuse mapM_/map" #-}

module Main (main) where

import Data.Function (on)
import Data.List (find, group, groupBy, minimumBy, sort, sortBy, uncons)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (dijkstra)
import DirectionalKeypad (DirectionalKey, directionalKeypad)
import GHC.Base (maxInt)
import GHC.Stack (HasCallStack)
import Grid (Grid (..), isEast, isNorth, isSouth, isWest)
import GridItem (GridItem)
import Node (Graph (..))
import NumericKeypad (NumericKey, numericKeypad)

toPairs :: (HasCallStack) => [a] -> [(a, a)]
toPairs [] = []
toPairs [a] = error "Singleton"
toPairs [a, b] = [(a, b)]
toPairs (a : b : rest) = (a, b) : toPairs (b : rest)

pairToDirection :: (HasCallStack, Eq a, Grid a) => (a, a) -> DirectionalKey
pairToDirection (from, to)
  | to `isNorth` from = u
  | to `isEast` from = r
  | to `isSouth` from = d
  | to `isWest` from = l
  | otherwise = error "Not neighbor"
  where
    [a, u, d, l, r] = directionalKeypad

toDirectionalPath :: (HasCallStack, Eq a, Grid a) => [a] -> [DirectionalKey]
toDirectionalPath path = map pairToDirection (toPairs path)

navigateSequence :: (HasCallStack, Eq n, Ord n, Graph n Int, Grid n) => [n] -> [n] -> [[[DirectionalKey]]]
navigateSequence keys keypad = map (\(start, end) -> pressKey start end keypad) (toPairs keys)

moveToKey :: (HasCallStack, Eq n, Ord n, Graph n Int, Grid n) => n -> n -> [n] -> [[DirectionalKey]]
moveToKey start end keypad = map toDirectionalPath (snd (dijkstra keypad start end maxInt))

pressKey :: (HasCallStack, Eq n, Ord n, Graph n Int, Grid n) => n -> n -> [n] -> [[DirectionalKey]]
pressKey start end keypad = map (++ take 1 directionalKeypad) (moveToKey start end keypad)

enterKeys :: (HasCallStack, Eq n, Ord n, Graph n Int, Grid n, Show n) => [n] -> [n] -> Int -> [DirectionalKey]
enterKeys keys keypad 0 = toDirectionalPath keys
enterKeys keys keypad maxDepth =
  traceShow
    (maxDepth, keys)
    ( concatMap
        ( \(start, end) ->
            let paths = pressKey start end keypad
             in if null paths
                  then []
                  else
                    minimumBy (compare `on` length) (map (\p -> enterKeys p directionalKeypad (maxDepth - 1)) paths)
        )
        (toPairs keys)
    )

part1 :: [[NumericKey]] -> [[DirectionalKey]]
part1 sequences =
  map
    ( \s ->
        let sequencePaths = navigateSequence s numericKeypad
         in concatMap
              ( \paths ->
                  minimumBy
                    (compare `on` length)
                    (map ((\path -> enterKeys path directionalKeypad 2) . (head directionalKeypad :)) paths)
              )
              sequencePaths
    )
    sequences

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
    -- putStrLn "Test"
    -- (mapM_ . mapM_)
    --   print
    --   ( map
    --       ( \k1 ->
    --           map (\k2 -> (k1, k2, moveToKey k1 k2 directionalKeypad)) directionalKeypad
    --       )
    --       directionalKeypad
    --   )
    -- putStrLn ""

    [nA, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] = numericKeypad
    [ndA, nU, nD, nL, nR] = directionalKeypad

    processInput :: (HasCallStack) => String -> [[NumericKey]]
    processInput contents =
      map
        ( (nA :)
            . map
              ( \ch -> case ch of
                  'A' -> nA
                  '0' -> n0
                  '1' -> n1
                  '2' -> n2
                  '3' -> n3
                  '4' -> n4
                  '5' -> n5
                  '6' -> n6
                  '7' -> n7
                  '8' -> n8
                  '9' -> n9
                  _ -> error "Invalid char"
              )
        )
        (lines contents)
