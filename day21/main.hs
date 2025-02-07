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

module Main (main) where

import Data.Function (on)
import Data.List (find, group, groupBy, minimumBy, sort, sortBy, uncons)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (Path (..), Paths (..), dijkstra, fromPath, pathHead, pathsHead)
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

toDirectionalPath :: (HasCallStack, Eq a, Grid a) => Path a -> Path DirectionalKey
toDirectionalPath (Path path) = Path (map pairToDirection (toPairs path))

navigateSequence :: (HasCallStack, Eq n, Ord n, Graph n Int) => [n] -> [n] -> [Paths n]
navigateSequence keys keypad = map (\(start, end) -> moveToKey start end keypad) (toPairs keys)

moveToKey :: (HasCallStack, Eq n, Ord n, Graph n Int) => n -> n -> [n] -> Paths n
moveToKey start end keypad = snd (dijkstra keypad start end maxInt)

newtype NumericRobotPaths = NumericRobotPaths (Paths NumericKey)
  deriving (Show)

numericRobot :: (HasCallStack) => NumericKey -> NumericKey -> NumericRobotPaths
numericRobot start end = NumericRobotPaths (moveToKey start end numericKeypad)

directionalRobot :: (HasCallStack) => DirectionalKey -> DirectionalKey -> Paths DirectionalKey
directionalRobot start end = moveToKey start end directionalKeypad

directionalHuman :: (HasCallStack) => DirectionalKey -> DirectionalKey -> Paths DirectionalKey
directionalHuman start end = moveToKey start end directionalKeypad

part1 sequences = sequences

part2 input = input

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  -- let part1_test = part1 test
  -- print part1_test -- Expected: ?
  -- print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?

  putStrLn "numericRobot"
  let num = numericRobot n2 n9
  print num
  print (Paths ((\(NumericRobotPaths (Paths paths)) -> map toDirectionalPath paths) num))
  putStrLn ""

  putStrLn "directionalRobot"
  let dirR = directionalRobot nD ndA
  print dirR
  print (Paths ((\(Paths paths) -> map toDirectionalPath paths) dirR))
  putStrLn ""

  putStrLn "directionalHuman"
  let dirH = directionalHuman ndA nL
  print dirH
  print (Paths ((\(Paths paths) -> map toDirectionalPath paths) dirH))
  putStrLn ""
  where
    -- putStrLn "navigateSequence"
    -- mapM_ print (navigateSequence (head test) numericKeypad)
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
