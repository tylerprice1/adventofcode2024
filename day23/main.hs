module Main (main) where

import Control.Monad.ST (ST, runST)
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

findInterconnected :: Int -> Computer -> LAN -> [Set.Set Computer]
findInterconnected 0 _ _ = []
findInterconnected 1 start _ = [Set.singleton start]
findInterconnected count start (LAN lan) =
  let count_1 = count - 1
      connections = (Map.!) lan start
      interconnections = concatMap (\c -> findInterconnected count_1 c (LAN lan)) connections
      valid =
        filter
          (\cs -> Set.size cs == count_1 && start `Set.notMember` cs && all (`Set.member` connections) cs)
          interconnections
   in map (start `Set.insert`) valid

getAllInterconnected :: Int -> LAN -> [Set.Set Computer]
getAllInterconnected count lan = unique $ concatMap (\computer -> findInterconnected count computer lan) (Map.keys (connections lan))

part1 :: LAN -> Int
part1 input = length $ filter (any ((== 't') . head . name)) $ getAllInterconnected 3 input

part2 :: LAN -> (Int, [Set.Set Computer])
part2 input =
  last $
    takeWhile (not . null . snd) $
      map (\count -> traceShow count (count, getAllInterconnected count input)) [1 ..]

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: ?
  print (part1 input) -- Expected: ?
  --
  putStrLn "\n----- Part 2 -----"
  mapM_ print (part2 test) -- Expected: ?
  mapM_ print (part2 input) -- Expected: ?

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
