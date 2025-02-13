module Main (main) where

import Data.Foldable (Foldable (..))
import Data.List (sort)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (traceShow, traceShowId)

newtype Computer = Computer {name :: String}
  deriving (Eq, Ord)

instance Show Computer where
  show (Computer s) = s

newtype LAN = LAN {connections :: Map.Map Computer [Computer]}
  deriving (Show)

part1 input =
  length $
    filter (any ((\(ch : _) -> ch == 't') . name)) $
      Set.toList . Set.fromList $
        sort $
          map sort (concatMap (\k -> findInterconnected 3 k input) (Map.keys (connections input)))

part2 input = input

findInterconnected :: Int -> Computer -> LAN -> [[Computer]]
findInterconnected 0 _ _ = []
findInterconnected count start (LAN lan) =
  let connections = (Map.!) lan start
   in (sort . Set.toList . Set.fromList)
        ( concatMap
            ( \c ->
                let inter = findInterconnected (count - 1) c (LAN lan)
                 in if null inter
                      then [[start]]
                      else map (start :) (filter (all (`elem` connections)) inter)
            )
            connections
        )

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  -- mapM_ print (Map.toList (connections test))
  -- print (part1 test) -- Expected: ?
  print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?

processInput :: [Char] -> LAN
processInput contents =
  let lns = lines contents
      connections =
        map
          ( \(ch1 : ch2 : '-' : ch3 : ch4 : "") ->
              (Computer [ch1, ch2], Computer [ch3, ch4])
          )
          lns
      lan =
        foldr
          ( \(c1, c2) m ->
              let m' = Map.alter (Just . maybe [c1] (c1 :)) c2 m
                  m'' = Map.alter (Just . maybe [c2] (c2 :)) c1 m'
               in m''
          )
          Map.empty
          connections
   in LAN (Map.map sort lan)
