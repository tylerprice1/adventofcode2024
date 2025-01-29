{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main (main) where

import Control.DeepSeq (deepseq)
import Data.Function (on)
import Data.List (intercalate, intersperse, minimumBy)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow, traceShowId)
import Dijkstra (dijkstra)
import GHC.Base (maxInt)
import GHC.List (foldl')
import Grid (Grid (..), GridItem (..), findGridItem, isEast, isNorth, isSouth, isWest)

type NumericGridItem = GridItem Char Int

data NumericKeypad = NumericKeypad
  { getNumericA :: NumericGridItem,
    get0 :: NumericGridItem,
    get1 :: NumericGridItem,
    get2 :: NumericGridItem,
    get3 :: NumericGridItem,
    get4 :: NumericGridItem,
    get5 :: NumericGridItem,
    get6 :: NumericGridItem,
    get7 :: NumericGridItem,
    get8 :: NumericGridItem,
    get9 :: NumericGridItem
  }

getNumericKeyNodes :: NumericKeypad -> [NumericGridItem]
getNumericKeyNodes (NumericKeypad a n0 n1 n2 n3 n4 n5 n6 n7 n8 n9) = [a, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9]

numericKeypadToGrid :: NumericKeypad -> Grid Char Int
numericKeypadToGrid (NumericKeypad a n0 n1 n2 n3 n4 n5 n6 n7 n8 n9) = Grid [a, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9]

type DirectionalGridItem = GridItem Char Int

data DirectionalKeypad = DirectionalKeypad
  { getDirectionalA :: DirectionalGridItem,
    getU :: DirectionalGridItem,
    getD :: DirectionalGridItem,
    getL :: DirectionalGridItem,
    getR :: DirectionalGridItem
  }

directionalKeypadToGrid :: DirectionalKeypad -> Grid Char Int
directionalKeypadToGrid (DirectionalKeypad a nu nd nl nr) = Grid [a, nu, nd, nl, nr]

type Path a = [a]

type PathSequence a = [Path a]

navigateToNumericKey :: NumericKeypad -> Char -> Char -> [Path NumericGridItem]
navigateToNumericKey numericKeypad startCh endCh =
  let asGrid = numericKeypadToGrid numericKeypad
      (Grid nodes) = asGrid
      !start = fromJust (findGridItem asGrid startCh)
      !end = fromJust (findGridItem asGrid endCh)
   in snd (dijkstra nodes start end maxInt)

navigateToDirectionalKey :: DirectionalKeypad -> Char -> Char -> [Path DirectionalGridItem]
navigateToDirectionalKey directionalKeypad startCh endCh =
  let asGrid = directionalKeypadToGrid directionalKeypad
      (Grid nodes) = asGrid
      !start = fromJust (findGridItem asGrid startCh)
      !end = fromJust (findGridItem asGrid endCh)
   in snd (dijkstra nodes start end maxInt)

gridPathToDirectionalPath :: DirectionalKeypad -> [GridItem Char Int] -> Path DirectionalGridItem
gridPathToDirectionalPath _ [] = error "Empty"
gridPathToDirectionalPath _ [_] = [] -- error "Singleton"
gridPathToDirectionalPath directionalKeypad (first : second : rest)
  | second == first = gridPathToDirectionalPath directionalKeypad (second : rest)
  | second `isNorth` first = u : gridPathToDirectionalPath directionalKeypad (second : rest)
  | second `isEast` first = r : gridPathToDirectionalPath directionalKeypad (second : rest)
  | second `isWest` first = l : gridPathToDirectionalPath directionalKeypad (second : rest)
  | second `isSouth` first = d : gridPathToDirectionalPath directionalKeypad (second : rest)
  | otherwise = error (show second ++ " is not neighbor of " ++ show first)
  where
    (DirectionalKeypad _ u d l r) = directionalKeypad

toPairs :: (Eq a, Show a) => [a] -> [(a, a)]
toPairs [] = []
toPairs [a] = error ("Singleton: " ++ show a)
toPairs [a, b] = [(a, b)]
toPairs (a : b : rest) = if a /= b then (a, b) : toPairs (b : rest) else toPairs (b : rest)

expandSecondDirectionalPath :: Path DirectionalGridItem -> DirectionalKeypad -> Path DirectionalGridItem
expandSecondDirectionalPath path directionalKeypad =
  let pairs = toPairs (getDirectionalA directionalKeypad : path)
   in concatMap
        ( \(start, end) ->
            head (navigateToDirectionalKey directionalKeypad (getValue start) (getValue end))
        )
        pairs

expandFirstDirectionalPath path directionalKeypad =
  let pairs = toPairs (getDirectionalA directionalKeypad : path)
   in {- minimumBy
        (compare `on` length)
         -} ( map
                ( \(start, end) ->
                    map
                      ( \path ->
                          expandSecondDirectionalPath
                            ( gridPathToDirectionalPath directionalKeypad path
                                ++ [getDirectionalA directionalKeypad]
                            )
                            directionalKeypad
                      )
                      (navigateToDirectionalKey directionalKeypad (getValue start) (getValue end))
                )
                pairs
            )

expandNumericPath path directionalKeypad =
  expandFirstDirectionalPath
    (gridPathToDirectionalPath directionalKeypad path ++ [getDirectionalA directionalKeypad])
    directionalKeypad

pressNumericKey start end numericKeypad directionalKeypad =
  {- minimumBy
    (compare `on` length) -}
  ( map
      (\numericPath -> expandNumericPath numericPath directionalKeypad)
      (navigateToNumericKey numericKeypad start end)
  )

-- part1 :: ([String], NumericKeypad, DirectionalKeypad) -> [[[[PathSequence DirectionalGridItem]]]]
part1 (sequences, numericKeypad, directionalKeypad) =
  map
    ( \sequence ->
        let directionalA = getDirectionalA directionalKeypad
            -- numeric robot
            numericDirectionalPathSequences =
              map
                (\(start, end) -> pressNumericKey start end numericKeypad directionalKeypad)
                (toPairs ('A' : sequence))
         in numericDirectionalPathSequences
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
  mapM_ print (zip (let (t, _, _) = test in t) (part1 test)) -- Expected: ?
  -- print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?

processInput :: String -> ([String], NumericKeypad, DirectionalKeypad)
processInput contents = (lines contents, numericKeypad, directionalKeypad)
  where
    edge node = (1, node)

    -- - +---+---+---+
    -- - | 7 | 8 | 9 |
    -- - +---+---+---+
    -- - | 4 | 5 | 6 |
    -- - +---+---+---+
    -- - | 1 | 2 | 3 |
    -- - +---+---+---+
    -- -     | 0 | A |
    -- -     +---+---+
    numericKeypad :: NumericKeypad
    numericKeypad = NumericKeypad nA n0 n1 n2 n3 n4 n5 n6 n7 n8 n9
      where
        nA = GridItem 'A' (Just e3) Nothing Nothing (Just e0)
        n0 = GridItem '0' (Just e2) (Just eA) Nothing Nothing
        n1 = GridItem '1' (Just e4) (Just e2) Nothing Nothing
        n2 = GridItem '2' (Just e5) (Just e3) (Just e0) (Just e1)
        n3 = GridItem '3' (Just e6) Nothing (Just eA) (Just e2)
        n4 = GridItem '4' (Just e7) (Just e5) (Just e1) Nothing
        n5 = GridItem '5' (Just e8) (Just e6) (Just e2) (Just e4)
        n6 = GridItem '6' (Just e9) Nothing (Just e3) (Just e5)
        n7 = GridItem '7' Nothing (Just e8) (Just e4) Nothing
        n8 = GridItem '8' Nothing (Just e9) (Just e5) (Just e7)
        n9 = GridItem '9' Nothing Nothing (Just e6) (Just e8)

        eA = edge nA
        e0 = edge n0
        e1 = edge n1
        e2 = edge n2
        e3 = edge n3
        e4 = edge n4
        e5 = edge n5
        e6 = edge n6
        e7 = edge n7
        e8 = edge n8
        e9 = edge n9

    -- -     +---+---+
    -- -     | ^ | A |
    -- - +---+---+---+
    -- - | < | v | > |
    -- - +---+---+---+
    directionalKeypad :: DirectionalKeypad
    directionalKeypad = DirectionalKeypad nA nU nD nL nR
      where
        nA = GridItem 'A' Nothing Nothing (Just eR) (Just eU)
        nU = GridItem '^' Nothing (Just eA) (Just eD) Nothing
        nL = GridItem '<' Nothing (Just eD) Nothing Nothing
        nR = GridItem '>' (Just eA) Nothing Nothing (Just eD)
        nD = GridItem 'v' (Just eU) (Just eR) Nothing (Just eL)

        eA = edge nA
        eU = edge nU
        eL = edge nL
        eR = edge nR
        eD = edge nD
