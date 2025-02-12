{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Main (main) where

import Data.List (sort)
import Data.Map qualified as Map
import Debug.Trace (trace, traceShow, traceShowId)

data Computer = Computer {name :: String, connections :: [Computer]}

connectTo from (Computer n cs) = Computer n (sort (from : cs))

instance Eq Computer where
  (==) (Computer a _) (Computer b _) = a == b

instance Ord Computer where
  compare (Computer a _) (Computer b _) = compare a b

instance Show Computer where
  show (Computer s cs) =
    "Computer "
      ++ s
      ++ " -> "
      ++ show (map name cs)

findInterconnected :: Int -> Computer -> [[Computer]]
findInterconnected 0 _ = []
findInterconnected count start =
  traceShow
    (count, start)
    concatMap
    (map (start :) . findInterconnected (count - 1))
    (connections start)

part1 = map (findInterconnected 3 . traceShowId)

-- let m =
--       foldr
--         ( \connection m ->
--             let (Connection c1 c2) = connection
--                 m' = Map.alter (Just . maybe [connection] (connection :)) c2 m
--                 m'' = Map.alter (Just . maybe [connection] (connection :)) c1 m'
--              in m''
--         )
--         (Map.empty :: Map.Map Computer [Connection])
--         input
--  in LAN m

part2 input = input

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  putStrLn "Test data:"
  mapM_ print test
  putStrLn ""
  mapM_ print (part1 (take 1 test)) -- Expected: ?
  -- mapM_ print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?

processInput :: String -> [Computer]
processInput contents =
  let lns = lines contents
      readline (ch1 : ch2 : '-' : ch3 : ch4 : "") = (Computer [ch1, ch2] [], Computer [ch3, ch4] [])
      connectionless = map readline (lines contents)

      connectionlessComputersMap =
        foldr
          (\(c1, c2) m -> Map.insert (name c2) c2 (Map.insert (name c1) c1 m))
          Map.empty
          connectionless

      computersMap =
        foldr
          ( \(c1, c2) m ->
              let m' = Map.adjust (c1 `connectTo`) (name c2) m
                  m'' = Map.adjust (c2 `connectTo`) (name c1) m'
               in m''
          )
          connectionlessComputersMap
          connectionless
      computers = Map.elems computersMap
   in computers
