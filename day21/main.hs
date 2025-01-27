module Main (main) where

import Control.DeepSeq (deepseq)
import Data.Maybe (fromJust)
import Dijkstra (dijkstra)
import GHC.Base (maxInt)
import Grid (Grid (..), GridItem (..), findGridItem, isEast, isNorth, isSouth, isWest)

data NumericKeypad = NumericKeypad
  { getNumericA :: GridItem Char Int,
    get0 :: GridItem Char Int,
    get1 :: GridItem Char Int,
    get2 :: GridItem Char Int,
    get3 :: GridItem Char Int,
    get4 :: GridItem Char Int,
    get5 :: GridItem Char Int,
    get6 :: GridItem Char Int,
    get7 :: GridItem Char Int,
    get8 :: GridItem Char Int,
    get9 :: GridItem Char Int
  }

numericKeypadToGrid :: NumericKeypad -> Grid Char Int
numericKeypadToGrid (NumericKeypad a n0 n1 n2 n3 n4 n5 n6 n7 n8 n9) = Grid [a, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9]

data DirectionalKeypad = DirectionalKeypad
  { getDirectionalA :: GridItem Char Int,
    getU :: GridItem Char Int,
    getD :: GridItem Char Int,
    getL :: GridItem Char Int,
    getR :: GridItem Char Int
  }

directionalKeypadToGrid :: DirectionalKeypad -> Grid Char Int
directionalKeypadToGrid (DirectionalKeypad a nu nd nl nr) = Grid [a, nu, nd, nl, nr]

navigateSequence :: Grid Char Int -> String -> [[GridItem Char Int]]
navigateSequence _ "" = error "Empty"
navigateSequence _ [ch] = error ("Singleton: " ++ show ch)
navigateSequence !grid (startCh : endCh : !s) =
  let (Grid !gridItems) = grid
      !start = fromJust (findGridItem grid startCh)
      !end = fromJust (findGridItem grid endCh)
      (_, !path) = dijkstra gridItems start end maxInt
   in case s of
        "" -> [path]
        _ ->
          let (next : rest) = navigateSequence grid (endCh : s)
           in path : tail next : rest

pathToDirs :: DirectionalKeypad -> [GridItem Char Int] -> [GridItem Char Int]
pathToDirs _ [] = error "Empty"
pathToDirs _ [_] = error "Only one"
pathToDirs directionalKeypad (first : second : rest)
  | first `isNorth` second = u : pathToDirs directionalKeypad (second : rest)
  | first `isEast` second = r : pathToDirs directionalKeypad (second : rest)
  | first `isWest` second = l : pathToDirs directionalKeypad (second : rest)
  | first `isSouth` second = d : pathToDirs directionalKeypad (second : rest)
  | otherwise = error "No neighbors"
  where
    (DirectionalKeypad _ u d l r) = directionalKeypad

part1 :: ([String], NumericKeypad, DirectionalKeypad) -> [[[GridItem Char Int]]]
part1 (sequences, numericKeypad, directionalKeypad) =
  let asGrid = numericKeypadToGrid numericKeypad
   in map
        ( \s ->
            let paths = navigateSequence asGrid (('A' : s))
             in paths
            -- _ =
            --   foldr
            --     ( \path dirs ->
            --         pathToDirs directionalKeypad path ++ dirs
            --     )
            --     []
            --     paths
        )
        sequences

-- map
--   (\s -> foldr id s)
--   []
--   sequences

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
