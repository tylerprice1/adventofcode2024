module Main (main) where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (Foldable (..))
import Data.List (sort)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (traceShow, traceShowId)

newtype Computer = Computer {name :: String}
  deriving (Eq, Ord)

instance Show Computer where
  show (Computer s) = s

newtype LAN = LAN {connections :: Map.Map Computer (Set.Set Computer)}
  deriving (Show)

type Cache = Map.Map (Int, Computer) [Set.Set Computer]

unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

findInterconnected :: Int -> Computer -> LAN -> Cache -> ([Set.Set Computer], Cache)
findInterconnected 0 _ _ cache = ([], cache)
findInterconnected 1 start _ cache = ([Set.singleton start], cache)
findInterconnected count start (LAN lan) cache = case Map.lookup (count, start) cache of
  Just cs -> (cs, cache)
  Nothing ->
    let count_1 = count - 1
        connections = (Map.!) lan start
        (interconnections, cache') =
          foldr'
            ( \c (acc, cache) ->
                let (cs, cache') = findInterconnected count_1 c (LAN lan) cache
                 in (cs ++ acc, cache')
            )
            ([], cache)
            connections

        valid =
          filter
            (\cs -> start `Set.notMember` cs && all (`Set.member` connections) cs && Set.size cs == count_1)
            interconnections

        result = unique $ map (start `Set.insert`) valid
     in (result, Map.insert (count, start) result cache')

getAllInterconnected :: Int -> LAN -> Cache -> ([Set.Set Computer], Cache)
getAllInterconnected count lan cache =
  first unique $
    foldr'
      ( \computer (acc, cache) ->
          let (cs, cache') = findInterconnected count computer lan cache
           in (cs ++ acc, cache')
      )
      ([], cache)
      (Map.keys (connections lan))

part1 :: LAN -> Cache -> (Int, Cache)
part1 lan cache =
  let (connections, cache') = getAllInterconnected 3 lan cache
   in (length $ filter (any ((== 't') . head . name)) connections, cache')

part2 :: LAN -> Cache -> ([Set.Set Computer], Cache)
part2 lan cache =
  let (allInterconnectedPerCount, cache') =
        foldr'
          ( \count (acc, cache) ->
              let (cs, cache') = getAllInterconnected count lan cache
               in (cs : traceShow count acc, cache')
          )
          ([], cache)
          (take (Map.size (connections lan)) [1 ..])
   in (last $ takeWhile (not . null) allInterconnectedPerCount, cache')

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  let testCache = Map.empty
  let inputCache = Map.empty

  putStrLn "\n----- Part 1 -----"
  let (part1_test, testCache') = part1 test testCache
  print (7, part1_test) -- Expected: 7
  --
  let (part1_input, inputCache') = part1 input inputCache
  print (1077, part1_input) -- Expected: 1077
  --
  putStrLn "\n----- Part 2 -----"
  let (part2_test, _) = part2 test testCache'
  mapM_ print part2_test -- Expected: co,de,ka,ta
  --
  let (part2_input, _) = part2 input inputCache'
  mapM_ print part2_input -- Expected: ?

processInput :: String -> LAN
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
   in LAN (Map.map Set.fromList lan)
